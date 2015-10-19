# Set Seed
set.seed (100)

# Load Libraries
library(shiny)
library (ISLR)
library(splines)

# Build Training and Testing data
inTrain <- sample (1: nrow(Wage), 0.6 * nrow(Wage))
training <- Wage[inTrain,]
testing  <- Wage[-inTrain,]

library(dplyr)

testingNew <- arrange(testing, age)

agelims <- range(Wage$age)

# Polynomial Regression
fit.poly <- lm(wage~poly(age ,4) ,data=training)
preds.poly <- predict (fit.poly ,newdata = testingNew, se=TRUE)
mse.poly <- mean((preds.poly$fit - testingNew$wage)^2)
poly.se.bands <- cbind(preds.poly$fit + 2 * preds.poly$se.fit, 
                         preds.poly$fit - 2 * preds.poly$se.fit)
max.wage.poly <- max(preds.poly$fit)


# Splines
fit.splines <- lm(wage ~ bs(age ,knots =c(25 ,40 ,60) ),data=training)
preds.splines <- predict (fit.splines ,newdata = testingNew, se=TRUE)
mse.splines <- mean((preds.splines$fit - testingNew$wage)^2)
splines.se.bands <- cbind(preds.splines$fit + 2 * preds.splines$se.fit, 
                         preds.splines$fit - 2 * preds.splines$se.fit)
max.wage.splines <- max(preds.splines$fit)


# Natural Splines
fit.naturalsplines <- lm(wage ~ ns(age ,knots =c(25 ,40 ,60) ),data=training)
preds.naturalsplines <- predict (fit.naturalsplines ,newdata = testingNew, se=TRUE)
mse.naturalsplines <- mean((preds.naturalsplines$fit - testingNew$wage)^2)
naturalsplines.se.bands <- cbind(preds.naturalsplines$fit + 2 * preds.naturalsplines$se.fit, 
                         preds.naturalsplines$fit - 2 * preds.naturalsplines$se.fit)
max.wage.naturalsplines <- max(preds.naturalsplines$fit)


# Smoothing Splines
fit.smoothingSplines <- smooth.spline(training$age, training$wage, df =16)
preds.smoothingSplines <- predict(fit.smoothingSplines ,newdata = testingNew, se=TRUE)
mse.smoothingSplines <- mean((predict(fit.smoothingSplines, x=testingNew$age)$y - testingNew$wage)^2)
max.wage.smoothingSplines <- max(preds.smoothingSplines$y)


# Local Regression (0.2 Span)
fit.localRegression <- loess (wage~age ,span =.2, data=training)
preds.localRegression <- predict (fit.localRegression ,newdata = testingNew, se=TRUE)
mse.localRegression <- mean((preds.localRegression$fit - testingNew$wage)^2)
localRegression.se.bands <- cbind(preds.localRegression$fit + 2 * preds.localRegression$se.fit, 
                         preds.localRegression$fit - 2 * preds.localRegression$se.fit)
max.wage.localRegression <- max(preds.localRegression$fit)


# Local Regression (0.5 Span)
fit.localRegression2 <- loess (wage~age ,span =.5, data=training)
preds.localRegression2 <- predict (fit.localRegression2 ,newdata = testingNew, se=TRUE)
mse.localRegression2 <- mean((preds.localRegression2$fit - testingNew$wage)^2)
localRegression2.se.bands <- cbind(preds.localRegression2$fit + 2 * preds.localRegression2$se.fit, 
                         preds.localRegression2$fit - 2 * preds.localRegression2$se.fit)
max.wage.localRegression2 <- max(preds.localRegression2$fit)


shinyServer(
  function(input, output) {

    output$outputIdMethod <- renderText(

       if (input$idMethod == 1) {

           ("Displayed : Testing Data = Dark Grey Points, Fitted Data  = Solid Blue Curve, 
            95% Confidence Interval = Dotted Blue Curves,
            Slider Age Value Prediction = Bright Green Point (Value noted as well)")

       } else if (input$idMethod == 2) {

           ("Displayed : Testing Data = Grey Points, 
            Fitted Data (Cubic Splines) = Solid Blue Curve,
            95% CI (Cubic Splines) = Dotted Blue Curves,
            Slider Age Value Prediction (Cubic Splines)= Bright Blue Point,
            Fitted Data (Natual Cubic Splines) = Solid Red Curve,
            95% CI (Natural Cubic Splines) = Dotted Red Curves,
            Slider Age Value Prediction (Natural Cubic Splines) = Bright Red Point")

       } else if (input$idMethod == 3) {

           ("Displayed : Testing Data = Dark Grey Points,
            Fitted Data  = Solid Red Curve,
            Slider Age Value Prediction = Bright Green Point (Value noted as well)")

       } else if (input$idMethod == 4) {

           ("Displayed : Testing Data = Grey Points,
            Fitted Data (Span = 0.2) = Solid Red Curve,
            95% CI (Span = 0.2) = Dotted Red Curves,
            Slider Age Value Prediction (Span = 0.2) = Bright Red Point,
            Fitted Data (Span = 0.5) = Solid Blue Curve,
            95% CI (Span = 0.5) = Dotted Blue Curves,
            Slider Age Value Prediction (Span = 0.5)= Bright Blue Point")

       } # end if
    ) # end render text

    output$printPoly <- renderPlot(

       if (input$idMethod == 1) {
          # Polynomial Regression
          plot(testingNew$age, testingNew$wage, xlim=range(testingNew$age) ,cex =.5, col ="darkgrey",
               xlab="Age", ylab="Wage ( thousand  $$$)")
          title ("Degree - 4 Polynomial")
          lines(testingNew$age, preds.poly$fit, lwd =2, col ="blue")
          matlines (testingNew$age , poly.se.bands ,lwd =1, col ="blue",lty =3)
          points(input$idAge, predict(fit.poly ,newdata = list(age=c(input$idAge))),
                 col="green", pch=19, cex=2)
          text(30, 240, paste("Overall MSE = ", round(mse.poly,2)), col="magenta")
          text(30, 220, paste("Maximum Wage = ", round(max.wage.poly,2)), col="red")
          text(30, 200, paste("Your Predicted Wage = ", 
                          round(predict(fit.poly ,newdata = list(age=c(input$idAge))), 
                                 2)), col="black")

       } else if (input$idMethod == 2) {
          # Regression Splines
          plot(testingNew$age, testingNew$wage, xlim=range(testingNew$age) ,cex =.5, col ="darkgrey",
               xlab="Age", ylab="Wage ( thousand  $$$)")
          abline(v=25,lty=3)
          abline(v=40,lty=3)
          abline(v=60,lty=3)
          title ("Splines")
          lines(testingNew$age, preds.splines$fit, lwd =2, col ="blue")
          matlines (testingNew$age , splines.se.bands ,lwd =1, col ="blue",lty =3)
          points(input$idAge, round(predict(fit.splines ,newdata = list(age=c(input$idAge))), 2),
                 col="blue", pch=19, cex=3)
          text(30, 240, paste("Overall MSE:Splines= ", round(mse.splines,2)), col="magenta")
          text(31, 220, paste("Maximum Wage:Splines = ", round(max.wage.splines,2)), col="red")
          text(31, 200, paste("Predicted Wage:Splines = ", 
                          round(predict(fit.splines ,newdata = list(age=c(input$idAge))), 
                                 2)), col="black")
          lines(testingNew$age, preds.naturalsplines$fit, lwd =2, col ="red")
          matlines (testingNew$age , naturalsplines.se.bands ,lwd =1, col ="red",lty =3)
          points(input$idAge, round(predict(fit.naturalsplines ,newdata = list(age=c(input$idAge))), 2),
                 col="red", pch=19, cex=1.5)
          text(65, 240, paste("Overall MSE:NaturalSplines= ", round(mse.naturalsplines,2)), col="magenta")
          text(65, 220, paste("Maximum Wage:NaturalSplines = ", round(max.wage.naturalsplines,2)), col="red")
          text(65, 200, paste("Predicted Wage:NaturalSplines = ", 
                          round(predict(fit.naturalsplines ,newdata = list(age=c(input$idAge))), 
                                 2)), col="black")
          legend("topright", legend=c("Natural Cubic Spline","Cubic Spline"),
                  col=c("red", "blue"), lwd=3)

       } else if (input$idMethod == 3) {
          # Smoothing Splines
          plot(testingNew$age, testingNew$wage ,xlim=agelims ,cex =.5, col ="darkgrey",
               xlab="Age", ylab="Wage ( thousand  $$$)")
          title ("Smoothing Spline")
          lines(preds.smoothingSplines ,col ="red",lwd =2)
          points(input$idAge, round(predict(fit.smoothingSplines, x=input$idAge)$y, 2),
                 col="green", pch=19, cex=2)
          text(30, 240, paste("Overall MSE = ", round(mse.smoothingSplines,2)), col="magenta")
          text(30, 220, paste("Maximum Wage = ", round(max.wage.smoothingSplines,2)), col="red")
          text(30, 200, paste("Your Predicted Wage = ", 
                          round(predict(fit.smoothingSplines, x=input$idAge)$y, 
                                 2)), col="black")
          legend("topright", legend=c("16 Degrees of Freedom"),
                  col=c("red"), lwd=3)

       } else if (input$idMethod == 4) {
          # Local Regression
          plot(testingNew$age, testingNew$wage, xlim=range(testingNew$age) ,cex =.5, col ="darkgrey",
               xlab="Age", ylab="Wage ( thousand  $$$)")
          title ("Local Regression")
          lines(testingNew$age, preds.localRegression$fit, lwd =2, col ="red")
          matlines (testingNew$age , localRegression.se.bands ,lwd =1, col ="red",lty =3)

          points(input$idAge, round(predict(fit.localRegression ,input$idAge), 2),
                 col="red", pch=19, cex=3)
          text(31, 240, paste("Overall MSE:0.2 Span = ", round(mse.localRegression,2)), col="magenta")
          text(32, 220, paste("Maximum Wage:0.2 Span = ", round(max.wage.localRegression,2)), col="red")
          text(32, 200, paste("Predicted Wage:0.2 Span = ", 
                          round(predict(fit.localRegression ,input$idAge), 
                                 2)), col="black")

          lines(testingNew$age, preds.localRegression2$fit, lwd =2, col ="blue")
          matlines (testingNew$age , localRegression2.se.bands ,lwd =1, col ="blue",lty =3)

          points(input$idAge, round(predict(fit.localRegression2 ,input$idAge), 2),
                 col="blue", pch=19, cex=1.5)
          text(65, 240, paste("Overall MSE:0.5 Span = ", round(mse.localRegression2,2)), col="magenta")
          text(65, 220, paste("Maximum Wage:0.5 Span = ", round(max.wage.localRegression2,2)), col="red")
          text(65, 200, paste("Predicted Wage:0.5 Span = ", 
                          round(predict(fit.localRegression2 ,input$idAge), 
                                 2)), col="black")

          legend("topright", legend=c("Span=0.2","Span=0.5"),
                  col=c("red","blue"), lwd=3)

       }   # end if
     )    # end render Plot
  }     # end function
)     # end shiny server