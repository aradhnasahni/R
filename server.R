#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({ 
   
    ##############################
    #Creating a logistic regression for titanic
    ##############################
    library(rpart)
    library(rpart.plot)
    library(titanic)
    mydf_train <- as.data.frame(titanic_train)
    summary(mydf_train)#let's see what's in the data
    View(mydf_train)
    
   
    ##############################
    library(titanic)
    mytree <- rpart(Survived ~ Pclass + Sex + Age+ SibSp, data = mydf_train, method = "class")
    rpart.plot(mytree)
    
   
    
  
  })
  
})
