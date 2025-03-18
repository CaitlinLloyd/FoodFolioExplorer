library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tidyr)
library(rsconnect)
library(plotly)

load("data.RData")


ui <- shinyUI(
              navbarPage("Food folio explorer",
                tabPanel(
                 titlePanel("Food item information"),
                  fluidRow(
                    # Select type of trend to plot
# Select type of trend to plot
  selectInput(inputId = "type","Which rating to plot?", choices =types, selected = "rating.fat"),h3("To see the food image and the rating distribution, click a bar")),
  fluidRow(
  splitLayout(cellWidths = c("50%", "25%","25%"), plotlyOutput(outputId = "foodplot", height = "1000px", width = "1200px"),
                plotOutput(outputId ="m_sd",height = "300px"),plotOutput(outputId = "image",height = "300px"))),

h2("The 17 rating dimensions form the 3 factors: Healthiness, Tastiness and Umami"),
h3("Click a point to see the food"),
plotlyOutput("plot3d",  width = 1000, height = 1000)),
                tabPanel(
                    titlePanel("Demographic characteristics and food preference"),h2("Look at associations between demographic characteristics and food preferences!"),
                    selectInput(inputId ="attribute", "Select attribute to plot", choices = c("Healthiness","Tastiness","Umami")),
                    h1("Plots"),
                    plotlyOutput(outputId = "all", height = "300px", width = "900px"),
                    
                      fluidRow(h2("Influence of healthiness, tastiness and umami on choice"),
                        selectInput(inputId ="char", "Select characteristic", choices = c("gender", "age", "cognitive.restraint","bmi","income","education")),
                    h1("Plots"),
                    h2("Influence of food attributes on choice"),
                    plotOutput(outputId = "demo_char", height = "300px", width = "900px")))
))


