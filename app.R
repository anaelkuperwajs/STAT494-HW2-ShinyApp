library(tidyverse)
library(tidymodels)
library(bslib)
library(shiny)

readRDS("final_model.rds")
data("lending_club")


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  numericInput(inputId = "funded_amnt", 
              label = "Funded Amount",
              value = 1000,
              min = 1000, 
              max = 40000),
  numericInput("int_rate",
               "Int Rate",
               value = 4,
               min = 4,
               max = 30),
  selectInput("term", 
              "Term", 
              choices = list(term_36 = "term_36", term_60 = "term_60")),
  selectInput("sub_grade", 
              "Sub Grade", 
              choices = list(A = "A", B = "B", C = "C", D = "D", E = "E", F = "F")),
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
               "Annual Income",
               value = 0,
               min = 0,
               max = 960000),
  selectInput("emp_length", 
              "Emp Length", 
              choices = list("emp_lt_1", "emp_1", "emp_ge_10", "emp_2", "emp_3", "emp_4", "emp_5", "emp_6",    
                             "emp_7", "emp_8", "emp_9", "emp_unk")),
  numericInput("delinq_2yrs",
               "2 year delinquency",
               value = 0,
               min = 0,
               max = 22),
  numericInput("inq_last_6mths",
               "Inq Last 6 Months",
               value = 0,
               min = 0,
               max = 5),
  numericInput("revol_util",
               "revol_util",
               value = 0,
               min = 0,
               max = 150),
  numericInput("open_il_6m",
               "open_il_6m",
               value = 0,
               min = 0,
               max = 32),
  numericInput("open_il_12m",
               "open_il_12m",
               value = 0,
               min = 0,
               max = 20),
  numericInput("open_il_24m",
               "open_il_24m",
               value = 0,
               min = 0,
               max = 30),
  numericInput("total_bal_il",
               "total_bal_il",
               value = 0,
               min = 0,
               max = 600000),
  numericInput("all_util",
               "all_util",
               value = 0,
               min = 0,
               max = 200),
  numericInput("inq_fi",
               "inq_fi",
               value = 0,
               min = 0,
               max = 15),
  numericInput("inq_last_12m",
               "inq_last_12m",
               value = 0,
               min = 0,
               max = 32),
  numericInput("num_il_tl",
               "num_il_tl",
               value = 0,
               min = 0,
               max = 85),
  numericInput("total_il_high_credit_limit",
               "total_il_high_credit_limit",
               value = 0,
               min = 0,
               max = 600000),
  selectInput("variable", 
              "Choose a variable to explore", 
              choices = list("funded_amnt", "int_rate", "annual_inc", "delinq_2yrs",
                             "inq_last_6mths", "revol_util", "open_il_6m", "open_il_12m",    
                             "open_il_24m", "total_bal_il", "all_util", "inq_fi",
                             "inq_last_12m", "num_il_tl", "total_il_high_credit_limit")),
  plotOutput(outputId = "cp_profile")
)

# * Another part of the user interface will allow them to choose a variable
# (you can limit this to only the quantitative variables) where they can explore
# the effects of changing that variable, holding all others constant.

# * After the user has entered all the required values, the output will be a
# CP profile with the the predicted value for the data that was entered,
# indicated by a point. I don't think the functions from `DALEX` and `DALEXtra`
# will work with a stacked model, so you'll likely have to (get to) do some of your own coding. 

server <- function(input, output) {
  output$cp_profile <- renderPlot({
    lending_club %>% 
      filter(name == input$name, 
             sex == input$sex) %>% 
      ggplot() +
      geom_line(aes(x = year, y = n)) +
      scale_x_continuous(limits = input$years) +
      theme_minimal()
  })
}

# cp_profile <- function(explainer, new_observation, var) {
#   cp <-
#     predict_profile(explainer = explainer, new_observation = new_observation, variables = var)
#   cp %>%
#     rename(yhat = '_yhat_') %>%
#     ggplot(aes_string(x = var, y = "yhat")) +
#     geom_line()
# }
# 
# ob <-
#   lending_club_testing %>% 
#   slice(4)
# 
# cp_profile(rf_explain, ob, "int_rate")


shinyApp(ui = ui, server = server)

# * Publish your app to [shinyapps.io](https://www.shinyapps.io/).
# There are instructions for doing that on the tutorial I linked to above.


