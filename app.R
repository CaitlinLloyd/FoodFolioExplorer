library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tidyr)
#library(rsconnect)
library(imager)

load("data.RData")
col_frame <- data.frame(col_att=c("#92D895","sandybrown", "plum"),attribute=c("Healthiness","Tastiness","Umami"))
# Define server function
server <- function(input, output) {
  
  # Subset data
  
  selections <- reactive({
    fr <- dat %>%
      select(input$type, stimulus) %>% group_by(stimulus) %>% summarise_at(1, c(mean),na.rm=TRUE) %>% data.frame()
  })
  
  
  
  fr <- reactive({
    dax <- means %>% dplyr::select(input$type, food,choice.prop)
    colnames(dax) <- c("rating","food","choice")
    as.data.frame(dax)
  })
  
  output$foodplot <- renderPlotly({
    par(mar = c(4, 4, 1, 1))
    plot.window <- function(xlim, ylim, log="", asp=NA, ...) {
      if (!all(is.finite(xlim))) xlim <- c(0,1)
      if (!all(is.finite(ylim))) ylim <- c(0,1)
      plot.window.orig(xlim, ylim, log="", asp=NA, ...)
    }
    
    Dax <- fr()
    
    figure <- plot_ly(Dax,x= ~food,  y = ~rating, type = 'bar')
    
    #figure  <- figure  %>% add_markers() 
    figure <- figure  %>% hide_legend()
    figure  %>% layout(scene = list(xaxis = list(title = "Food"),yaxis = list(title = input$type)))
  })
  
  
  output$m_sd <- renderPlot({
    click_data <- event_data("plotly_click")
    filtered_dat <- subset(dat, dat$food==click_data$x)
    mean_sd <- paste0("mean (sd)= ",round(mean(filtered_dat[[input$type]],na.rm=TRUE), 2)," (",round(sd(filtered_dat[[input$type]],na.rm=TRUE), 2),")")
    p <- ggplot(filtered_dat,aes_string(x=input$type)) + geom_histogram(stat="count",binwidth = 0.5,color="darkgreen") + ggtitle(mean_sd)
    p
  })
  
  output$image <- renderPlot({
    click_data <- event_data("plotly_click")
    filtered_dat <- subset(dat, dat$food==click_data$x)
    img <- paste0("stim/",filtered_dat$stimulus[1],".jpg")
    im <- imager::load.image(img)
    plot(im,axes=FALSE)
  })

  
  
  
  output$plot3d <- renderPlotly({
    
    par(mar = c(4, 4, 1, 1))
    plot.window <- function(xlim, ylim, log="", asp=NA, ...) {
      if (!all(is.finite(xlim))) xlim <- c(0,1)
      if (!all(is.finite(ylim))) ylim <- c(0,1)
      plot.window.orig(xlim, ylim, log="", asp=NA, ...)
    }
    
    fig <- plot_ly(means, x = ~Healthiness, y = ~Tastiness, z = ~Umami,name = ~food)
    
    fig <- fig %>% add_markers() 
    fig <- fig %>% hide_legend()
    
    fig %>% layout(scene = list(xaxis = list(title = 'Healthiness'),
                                
                                yaxis = list(title = 'Tastiness'),
                                
                                zaxis = list(title = 'Umami')))
    
  })
  
  
  
  
  
  
  
  fr_char <- reactive({
    daxa <- means %>% dplyr::select(input$attribute, food,choice.prop)
    colnames(daxa) <- c("food_attribute","food","choice")
    as.data.frame(daxa)
  })
  
  color_att<- reactive({
    col_att <- col_frame[col_frame$attribute==input$attribute,]$col_att
  as.character(col_att)
  })
  
  
  output$all <- renderPlotly({
    Daxa <- fr_char()
    cp <- color_att()
    Daxa$cp <- cp

    plot <- plot_ly(Daxa,x = ~food_attribute,  y = ~choice,name=~food,color = ~cp, colors = c(cp))
    
    #figure  <- figure  %>% add_markers() 
    plot  %>% hide_legend()
    #figure  %>% layout(scene = list(xaxis = list(title = "Food"),yaxis = list(title = input$type)))
  })
  
  
  
  output$demo_char <- renderPlot({
    c <- polycor::hetcor(health_select[[input$char]],health_select[[input$attribute]])
    c <- c$correlations[1,2]
    color_a <- color_att()
    if(class(health_select[[input$char]])=="character"){
      
      p <-  ggplot(health_select, aes_string(x = input$char, y =input$attribute)) + geom_boxplot(fill=color_a) + ggtitle(paste0(input$attribute, " and ", input$char, " correlation=",c))
      p <- p + theme_classic()
    }
    if(class(health_select[[input$char]])=="numeric"){
      p <-  ggplot(health_select, aes_string(x = input$char, y =input$attribute)) + geom_point(color=color_a) + geom_smooth(method="lm", color=color_a,fill=color_a) + ggtitle(paste0(input$attribute, " and ", input$char, " correlation=",c))
      p <- p + theme_classic()
      }
    p
  })
  
}


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
                        h2("Influence of food attributes on choice by demographic characteristics"),
                        plotOutput(outputId = "demo_char", height = "300px", width = "900px")))
  ))

shinyApp(ui = ui, server = server)



