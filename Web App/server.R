# Loading the necessary libraries
library(shiny)
library(HistData)
library(dplyr)
library(ggplot2)

# Loading the data

dataset <- GaltonFamilies

# 1st step: Pass inches to cm

## Setting the parameter for changing inches to centimeters
inchToCM = 2.54

dataset <- dataset %>% mutate(father=father*inchToCM,
                    mother=mother*inchToCM,
                    childHeight=childHeight*inchToCM)

# Applying a Linear Model

heightModel <- lm(childHeight ~ father + mother + gender, data=dataset)

shinyServer(function(input, output) {
  output$pText <- renderText({
    paste("Father's height is",
          strong(round(input$inFh, 1)),
          "cm, and mother's height is",
          strong(round(input$inMh, 1)),
          "cm, then:")
  })
  output$pred <- renderText({
    inputData <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(dataset$gender)))
    ch <- predict(heightModel, newdata=inputData)
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    paste0(em(strong(kid)),
           "'s predicted height is going to be around ",
           em(strong(round(ch))),
           " cm"
    )
  })
  output$Plot <- renderPlot({
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    inputData <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(dataset$gender)))
    ch <- predict(heightModel, newdata=inputData)
    yvals <- c("Father", kid, "Mother")
    inputData <- data.frame(
      x = factor(yvals, levels = yvals, ordered = TRUE),
      y = c(input$inFh, ch, input$inMh))
    ggplot(inputData, aes(x=x, y=y, color=c("Grey", "green", "black"), fill=c("Grey", "green", "black"))) +
      geom_bar(stat="identity", width=0.5) +
      xlab("") +
      ylab("Height (cm)") +
      theme_minimal() +
      theme(legend.position="none")
  })
})