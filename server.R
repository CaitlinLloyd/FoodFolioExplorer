library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tidyr)
library(rsconnect)
library(imager)


# dat <- read.csv("mTURK_March2021.csv")
# source("cleanup1.R")
# dat <- t
# types <- dat %>% dplyr::select(starts_with("rating"), "Healthiness",             "Tastiness",              "Umami","choice.rating") %>% colnames ()
# dat[,types] <- sapply(dat[,types], as.numeric)
# foods <- unique(dat$stimulus)
# characteristics <- c("none", colnames(dat)[60:95])
# fat <- c("all","by high and low")
# 
# mod <- lmerTest::lmer(formula=as.formula(paste0("choice.rating ~  (1 + Tastiness + Healthiness + Umami|subjectId) + (1|stimulus)")),dat=dat,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5), calc.derivs = FALSE), contrasts = c("contr.sum"))
# demo <- ranef(mod)$subjectId[-1,]
# demo$subjectId <- rownames(demo)
# health_select <- merge(health_select,demo,by="subjectId")
# health_select <- health_select[!duplicated(health_select$subjectId),]
# demo <- dat %>% select(subjectId,age2,gender,income,education,bmi,s.cognitive.restraint)
# health_select <- merge(health_select,demo)
# health_select <- health_select[!duplicated(health_select$subjectId),]
# health_select$age <- as.numeric(health_select$age2)
# health_select$cognitive.restraint <- health_select$s.cognitive.restraint
# 
# means <- dat %>% group_by(food) %>% summarize_at(types, mean,na.rm=TRUE)
# 
# choice <- dat %>% group_by(food,choice_bin) %>% count() %>% pivot_wider(1,names_from = 2,values_from = 3)
# choice$choice.prop <- choice$`1`/(choice$`0` + choice$`1`)
# means <- merge(means,choice,by="food")
# 
# save(health_select, dat,means,types, file = "data.RData")
        
load("data.RData")

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
    colnames(daxa) <- c("att","food","choice")
    as.data.frame(daxa)
  })
  
  
  output$all <- renderPlotly({
    Daxa <- fr_char()
    plot <- plot_ly(Daxa,x = ~att,  y = ~choice,name=~food,color = "red")
    
    #figure  <- figure  %>% add_markers() 
    plot  %>% hide_legend()
    #figure  %>% layout(scene = list(xaxis = list(title = "Food"),yaxis = list(title = input$type)))
  })
  
  
  
  output$demo_char <- renderPlot({
    c <- polycor::hetcor(health_select[[input$char]],health_select[[input$attribute]])
    c <- c$correlations[1,2]
    if(class(health_select[[input$char]])=="character"){
      p <-  ggplot(health_select, aes_string(x = (input$char), y =input$attribute)) + geom_boxplot() + ggtitle(paste0(input$attribute, " and ", input$char, " correlation=",c))
    }
    if(class(health_select[[input$char]])=="numeric"){
      p <-  ggplot(health_select, aes_string(x = input$char, y =input$attribute)) + geom_point() + geom_smooth(method="lm") + ggtitle(paste0(input$attribute, " and ", input$char, " correlation=",c))
    }
    p
  })
  
}




