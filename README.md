# STAT494-HW2-ShinyApp

You are going to create an app that allows a user to explore how the predicted probability of a loan being paid back (or maybe just the predicted class - either "good" or "bad") changes depending on the values of the predictor variables.

Specifically, you will do the following:

* Set up a separate project and GitHub repo for this app. Make sure the saved model from the previous problem is also in that folder. The app needs to be created in a file called *exactly* app.R that is also in the project folder.   
* At the top of the file, load any libraries you use in the app.  
* Use the `readRDS()` function to load the model.  
* You may want to load some of the data to use
* Create a user interface (using the various `*Input()` functions) where someone could enter values for each variable that feeds into the model. You will want to think hard about which types of `*Input()` functions to use. Think about how you can best prevent mistakes (eg. entering free text could lead to many mistakes). 
* Another part of the user interface will allow them to choose a variable (you can limit this to only the quantitative variables) where they can explore the effects of changing that variable, holding all others constant.  
* After the user has entered all the required values, the output will be a CP profile with the the predicted value for the data that was entered, indicated by a point. I don't think the functions from `DALEX` and `DALEXtra` will work with a stacked model, so you'll likely have to (get to) do some of your own coding. 
* Use the `bslib` to theme your shiny app!  
* Publish your app to [shinyapps.io](https://www.shinyapps.io/). There are instructions for doing that on the tutorial I linked to above.   
* Write a paragraph or two describing your app on your website! Link to the app and your GitHub repository in your post. Include a link to your post here.