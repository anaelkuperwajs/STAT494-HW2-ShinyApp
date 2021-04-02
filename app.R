library(tidyverse)
library(tidymodels)
library(stacks)
library(modeldata)
library(bslib)
library(rsconnect)
library(shiny)
library(ranger)
library(naniar)
library(lubridate)
library(moderndive)
library(vip)
library(DALEX) 
library(DALEXtra)
library(patchwork)

lending_club_final_model <- readRDS("final_model.rds")
data("lending_club")


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  numericInput(inputId = "funded_amnt", 
              label = "Funded amount",
              value = 1000,
              min = 1000, 
              max = 40000),
  numericInput("int_rate",
               "Interest Rate",
               value = 4,
               min = 4,
               max = 30),
  selectInput("term", 
              "The number of payments on the loan", 
              choices = list(term_36 = "term_36", term_60 = "term_60")),
  selectInput("sub_grade", 
              "Sub grade", 
              choices = levels(lending_club$sub_grade)),
  selectInput("addr_state", 
              "State", 
              choices = list(Alaska = "AK", Alabama = "AL", Arkansas = "AR", Arizona = "AZ", California = "CA",
                             Colorado = "CO", Connecticut = "CT", Washington_DC = "DC", Delaware = "DE",
                             Florida = "FL", Georgia = "GA", Hawaii = "HI", Idaho = "ID", Illinois = "IL",
                             Indiana = "IN", Kansas = "KS", Kentucky = "KY", Louisiana = "LA",
                             Massachussets = "MA", Maryland = "MD", Maine = "ME", Michigan = "MI",
                             Minnesota = "MN", Missouri = "MO", Mississippi = "MS", Montana = "MT",
                             North_Carolina = "NC", North_Dakota = "ND", Nebraska = "NE", New_Hampshire = "NH",
                             New_Jersey = "NJ", New_Mexico = "NM", Nevada = "NV", New_York = "NY",
                             Ohio = "OH", Oklahoma = "OK", Oregon = "OR", Pennsylvania = "PA",
                             Rhode_Island = "RI", South_Carolina = "SC", South_Dakota = "SD",
                             Tennessee = "TN", Texas = "TX", Utah = "UT", Virginia = "VA", Vermont = "VT",
                             Washington = "WA", Wisconsin = "WI", West_Virginia = "WV", Wyoming = "WY")),
  selectInput("verification_status", 
              "Verification Status", 
              choices = list(Not_Verified = "Not_Verified", Source_Verified = "Source_Verified", Verified = "Verified")),
  numericInput("annual_inc",
               "Annual income",
               value = 0,
               min = 0,
               max = 960000),
  selectInput("emp_length", 
              "Employment length in years", 
              choices = list("emp_lt_1", "emp_1", "emp_ge_10", "emp_2", "emp_3", "emp_4", "emp_5", "emp_6",    
                             "emp_7", "emp_8", "emp_9", "emp_unk")),
  numericInput("delinq_2yrs",
               "The number of delinquency incidents over the past 2 years",
               value = 0,
               min = 0,
               max = 22),
  numericInput("inq_last_6mths",
               "The number of inquiries in past 6 months",
               value = 0,
               min = 0,
               max = 5),
  numericInput("revol_util",
               "Revolving line utilization rate",
               value = 0,
               min = 0,
               max = 150),
  numericInput("open_il_6m",
               "Number of installment accounts opened in past 6 months",
               value = 0,
               min = 0,
               max = 32),
  numericInput("open_il_12m",
               "Number of installment accounts opened in past 12 months",
               value = 0,
               min = 0,
               max = 20),
  numericInput("open_il_24m",
               "Number of installment accounts opened in past 24 months",
               value = 0,
               min = 0,
               max = 30),
  numericInput("total_bal_il",
               "Total current balance of all installment accounts",
               value = 0,
               min = 0,
               max = 600000),
  numericInput("all_util",
               "Balance to credit limit on all trades",
               value = 0,
               min = 0,
               max = 200),
  numericInput("inq_fi",
               "Number of personal finance inquiries",
               value = 0,
               min = 0,
               max = 15),
  numericInput("inq_last_12m",
               "Number of credit inquiries in past 12 months",
               value = 0,
               min = 0,
               max = 32),
  numericInput("num_il_tl",
               "Number of installment accounts",
               value = 0,
               min = 0,
               max = 85),
  numericInput("total_il_high_credit_limit",
               "Total installment high credit/credit limit",
               value = 0,
               min = 0,
               max = 600000),
  selectInput("variable", 
              "Choose a variable to explore", 
              choices = list("Funded Amount" = "funded_amnt", "int_rate", "annual_inc", "delinq_2yrs",
                             "inq_last_6mths", "revol_util", "open_il_6m", "open_il_12m",    
                             "open_il_24m", "total_bal_il", "all_util", "inq_fi",
                             "inq_last_12m", "num_il_tl", "total_il_high_credit_limit")),
  plotOutput(outputId = "cp_profile")
)

#Currently setup for just int_rate, not using 'Choose a variable to explore'

server <- function(input, output) {
  output$cp_profile <- renderPlot({
    df <- data.frame(funded_amnt = input$funded_amnt,
                 term = input$term,
                 int_rate = input$int_rate,
                 sub_grade = input$sub_grade,
                 addr_state = input$addr_state,
                 verification_status = input$verification_status,
                 annual_inc = input$annual_inc,
                 emp_length = input$emp_length,
                 delinq_2yrs = input$delinq_2yrs,
                 inq_last_6mths = input$inq_last_6mths,
                 revol_util = input$revol_util,
                 open_il_6m = input$open_il_6m,
                 open_il_12m = input$open_il_12m,
                 open_il_24m = input$open_il_24m,
                 total_bal_il = input$total_bal_il,
                 all_util = input$all_util,
                 inq_fi = input$inq_fi,
                 inq_last_12m = input$inq_last_12m,
                 num_il_tl = input$num_il_tl,
                 total_il_high_credit_limit = input$total_il_high_credit_limit)
    
    min_var <- min(lending_club$int_rate)
    max_var <- max(lending_club$int_rate)
    
    obs_many <- df %>% 
      sample_n(size = 50, replace = TRUE) %>% 
      select(-int_rate) %>%
      mutate(int_rate = seq(min_var, max_var, length.out = 50))
    
    obs_many %>% 
      select(int_rate) %>% 
      bind_cols(
        predict(lending_club_final_model,
                new_data = obs_many, type = "prob")
      ) %>% 
      ggplot(aes(x = int_rate,
                 y = .pred_good)) +
      geom_line() +
      labs(y = "Predicted")
  })
}


shinyApp(ui = ui, server = server)
