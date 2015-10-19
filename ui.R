library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(width=6,
      strong("Fitting Models to Non-Linear Data", style = "font-size:12pt;"),
      strong(" ( By : Narendra S.Shukla, Oct-2015 )", style = "font-size:9pt; color:blue;"),
      p("When relationship between Predictors and 
         Response variable is Non-Linear, Linear models often 
         perform poorly. In this project, we demonstrate 4 Non-Linear Models built 
         against the same data-set.",style = "font-size:8pt;"),

      strong("What does {server.R} do :",style = "font-size:9pt;"),
      tags$ol( style = "font-size:8pt;", 
       tags$li("Load **Wage** Dataset from **ISLR** Library, 3000 observations of 11 variables"),
       tags$li("Train the Model to Predict **wage** as a function of **age** for each model"),
       tags$li("For selected Model, Display Chart of **Predicted Values**, 95% CI Bands and Overall MSE"),
       tags$li("Take **Age** input from **Slider**, Predict **Wage**. Display Predicted Wage **Point**"),
       tags$li("Calculate Maximum Wage and Display it in RED text")    
             ),
      strong("Instructions : ",style = "font-size:9pt;"),
      tags$ol(  style = "font-size:8pt;",    
      tags$li("Select one of the 4 Non-Linear Models. Default Model selected is **Polynomial Regression**"),
      tags$li("You will see corresponding Chart displayed, along with relevent information for that model"),
      tags$li("Select **Age** using Slider. Observe **Point** and **Your Predicted Wage**"),
      tags$li("Moving Slider **Age** values, moves **Predicted** point accordingly on the Chart"),
      tags$li("Switching between different models, displays new chart and new information")
             ),

      p(radioButtons('idMethod', 'Select Your Non-Linear Model', 
                   c("Polynomial Regression" = 1,
                     "Regression Splines" = 2,
                     "Smoothing Splines" = 3,
                     "Local Regression" = 4),
                   selected = 1, inline=TRUE)),
      tags$style(type='text/css', '#idMethod {font-size:10pt;}'),
      p(sliderInput('idAge', 'Select the Age',value = 35, min = 18, max = 80, step = 1))
   ),
    mainPanel(width=6,
        p(plotOutput('printPoly')),
        textOutput("outputIdMethod"),
        tags$style(type='text/css', '#outputIdMethod {font-size:7pt;}')
    )   # end mainPanel
  )  # end sidebarLayout
 )  # end fluidPage
)  # end shinyUI

