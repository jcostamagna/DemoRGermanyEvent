
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
require(ggplot2)
library(gridExtra)
library(lubridate)

require("RPostgreSQL")
require("knitr")
require(psych)
require(dplyr)
library(pspline)
#c("psych","knitr","RPostgreSQL","gridExtra")

headset_xlim <- c(-0.5,0.5)
headset_ylim <- c(-0.5,0.5)

drv <- dbDriver("PostgreSQL")
connection <- dbConnect(drv, dbname = "postgres",password="1way.Street",
                        host = "localhost", port = 5432,
                        user = "postgres")

perfectDriverSession = 392


shinyServer(function(input, output, session) {

  
  observeEvent(input$connect_db, {
    
    connect_db()
    # check for the cartable
    # result <- dbExistsTable(connection, "session")
    # if(result){
    #   message = "Connected to db"
    # } else {
    #   message = "Could not connect to db"
    # }
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = message)
    })
  
  # observeEvent(input$fetch_sessions, {
  #   
  # })
  
   session_data_pos_PDriver <- get_car_full_data(connection, perfectDriverSession)
   #session_data_pos_2nd <- get_car_full_data(connection, input$session_id_2nd)
   #session_data_pos_together <- get_car_full_data_two_sessions(connection, perfectDriverSession, input$session_id_2nd)
  
   #print(input$session_id_2nd)
  
  generalPlotsTogether <- function(idSession){
    
   
    session_data_pos <- get_car_full_data_two_sessions(connection, perfectDriverSession, idSession)
    #session_data_pos <- session_data_pos_together

    p8 <- ggplot(session_data_pos, aes( throttle, color=factor(idsession))) +
      geom_density()
    p9 <- ggplot(session_data_pos, aes( rpm, color=factor(idsession))) +
      geom_density()
    
    
    grid.arrange(p8,p9,
                 ncol=1,heights=c(4,4))
    
  }
  
  generalPlots <- function(idSession){
    
    session_data <- get_session_data(connection, idSession)
    
    p1 <- ggplot(data=session_data, aes(session_data$accelerator)) + geom_histogram()
    p3 <- ggplot(data=session_data, aes(session_data$steering)) + geom_histogram() +
      scale_x_continuous(limits = c(0, 1))
    
    p4 <- ggplot(data=session_data, aes(session_data$steering_diff)) + geom_histogram() +
      scale_x_continuous(limits = c(0,1))
    p5 <- ggplot(session_data, aes(x=steering, y=longitudinal)) +
      geom_point(shape=1)  + scale_x_continuous(limits = c(0, 1))
    p7 <- ggplot(session_data, aes(x = -rotationy, y = rotationx)) + stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
      scale_fill_gradientn(colours = terrain.colors(10)) + scale_x_continuous(limits = headset_xlim) +
      scale_y_continuous(limits = headset_ylim)
    
    
    grid.arrange(p1,p3,p4,p5,p7,
                 ncol=1,heights=c(4,4,4,4,4))
    
  }
  
  plot_sector_session <- function(idSession, beg_sector, end_sector){
    
    if (idSession == perfectDriverSession){
      session_data_pos <- session_data_pos_PDriver
    }else{
      session_data_pos <- get_car_full_data(connection, idSession)
    }
    
    sector_data <- get_sector_data(session_data_pos, beg_sector, end_sector)
    
    p1 <- ggplot(sector_data, aes(x=posx, y=posy, color=speed)) +
      geom_path( size = 0.75)  + scale_color_gradient(low="blue", high="red", limits = c(0,80))

    grid.arrange(p1,
                 ncol=1,heights=c(6))
    
  }
  
  plot_general_race <- function(idSession){
    
    connect_db()
    session_data_pos <- get_car_full_data(connection, idSession)
    

    p1 <- ggplot(session_data_pos, aes(x=session_data_pos$posx, y=session_data_pos$posy, color=speed)) +
      geom_point(shape=16, size = 0.75)+ scale_y_continuous(limits = c(-40, 0))  + scale_x_continuous(limits = c(-750, 750)) + 
      scale_color_gradient(low="blue", high="red", limits = c(0,80))
    
    
    
    grid.arrange(p1, ncol=1,heights=c(6))
    
  }
  
  plots_together_sector <- function(idSession1, idSession2, beg_sector, end_sector){
    
    connect_db()
    session_data_pos <- get_car_full_data_two_sessions(connection, idSession1, idSession2)
    #session_data_pos <- session_data_pos_together
    sector_data <- get_sector_data(session_data_pos, beg_sector, end_sector)
    
    #Sector
    p0 <- ggplot(sector_data, aes(x=distance, y=speed, color=factor(idsession))) +
      geom_path(size=0.5) + scale_color_manual(values = c("red", "black"))
    p1 <- ggplot(sector_data, aes(x=distance, y=brake, color=factor(idsession))) + 
      geom_path(size=1.0)  
    p2 <- ggplot(sector_data, aes(x=distance, y=throttle, color=factor(idsession))) + 
      geom_path()  
    p3 <- ggplot(sector_data, aes(x=distance, y=steering, color=factor(idsession))) + 
      geom_path()
    
    grid.arrange(p0,p1,p3,
                 ncol=1,heights=c(4,4,4))
    
  }
  
  output$distPlot_1st <- renderPlot({
    plot_general_race(perfectDriverSession)
    
  })
  
  # output$table <- DT::renderDataTable(DT::datatable({
  #   sessions
  # }))
  
  output$distPlot_2nd <- renderPlot({
    plot_general_race(input$session_id_2nd)
   
  })
  
  output$distPlot_together_sector1 <- renderPlot({
    #sector1
    plots_together_sector(perfectDriverSession, input$session_id_2nd, 500, 900)
    
  })
  
  output$distPlot_together_sector2 <- renderPlot({
    #sector2
    plots_together_sector(perfectDriverSession, input$session_id_2nd, 900, 1240)
    
  })
  
  output$distPlot_together_sector3 <- renderPlot({
    #sector3
    plots_together_sector(perfectDriverSession, input$session_id_2nd, 2606, 2940)
    
  })
  
  output$distPlot_sector1_1st <- renderPlot({
    plot_sector_session(perfectDriverSession, 500, 900)
  })
  
  output$distPlot_sector1_2nd <- renderPlot({
    plot_sector_session(input$session_id_2nd, 500, 900)
  })
  
  output$distPlot_sector2_1st <- renderPlot({
    plot_sector_session(perfectDriverSession, 900, 1240)
  })
  
  output$distPlot_sector2_2nd <- renderPlot({
    plot_sector_session(input$session_id_2nd, 900, 1240)
  })
  
  output$distPlot_sector3_1st <- renderPlot({
    plot_sector_session(perfectDriverSession, 2606, 2940)
  })
  
  output$distPlot_sector3_2nd <- renderPlot({
    plot_sector_session(input$session_id_2nd, 2606, 2940)
  })
  
  output$generalPlot_1st <- renderPlot({
    generalPlots(perfectDriverSession)
  })
  
  output$generalPlot_2nd <- renderPlot({
    generalPlots(input$session_id_2nd)
  })
  
  output$generalPlotTogether <- renderPlot({
    generalPlotsTogether(input$session_id_2nd)
  })
  
  output$timeTotal1st <- renderText({ 
    
    td <-seconds_to_period(max(session_data_pos_PDriver$currenttime,na.rm=TRUE))
    sprintf('%s', td)
  })
  
  output$timeTotal2nd <- renderText({ 
    session_data_pos <- get_car_full_data(connection, input$session_id_2nd)
    td <- seconds_to_period(max(session_data_pos$currenttime,na.rm=TRUE))
    sprintf('%s', td)
  })
  
 output$timeSector1_1st <- renderText({
   sector_data <- get_sector_data(session_data_pos_PDriver, 500, 900)
   td <-seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
   sprintf('%s', td)
 })
  
 output$timeSector1_2nd <- renderText({
   session_data_pos_2nd <- get_car_full_data(connection, input$session_id_2nd)
   sector_data <- get_sector_data(session_data_pos_2nd, 500, 900)
   td <- seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
   sprintf('%s', td)
 })

 output$timeSector2_1st <- renderText({
   sector_data <- get_sector_data(session_data_pos_PDriver, 900, 1240)
   td <-seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
   sprintf('%s', td)
 })

 output$timeSector2_2nd <- renderText({
   session_data_pos <- get_car_time_data(connection, input$session_id_2nd)
   sector_data <- get_sector_data(session_data_pos, 900, 1240)
   td <- seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
   sprintf('%s', td)
 })

 output$timeSector3_1st <- renderText({
   sector_data <- get_sector_data(session_data_pos_PDriver, 2606, 2940)
   td <-seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
   sprintf('%s', td)
 })

 output$timeSector3_2nd <- renderText({
   session_data_pos <- get_car_time_data(connection, input$session_id_2nd)
   sector_data <- get_sector_data(session_data_pos, 2606, 2940)
   td <- seconds_to_period(max(sector_data$currenttime,na.rm=TRUE) - min(sector_data$currenttime,na.rm=TRUE))
  sprintf('%s', td)
 })
  

})


killDbConnections <- function (drv) {
  
  all_cons <- dbListConnections(drv)
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

connect_db <- function(){
  
  drv <- dbDriver("PostgreSQL")

  
  
  if(isPostgresqlIdCurrent(connection)){
    return(TRUE)
  }
  

  
  killDbConnections(drv)
  

  
  #connection <- dbConnect(drv, dbname = "postgres",
  #                 host = "10.187.64.104", port = 5432,
  #                 user = "postgres")
  return(TRUE)
}


normalizar <- function(x){
  returnValue(x/65535)
}

get_session_data <- function(connection,session_id){
  
  connect_db()
  
  query <- "SELECT datavr.*,datasteering.steering,datasteering.accelerator,datasteering.slider1 AS brake FROM
  datasteering 
  JOIN datavr ON datasteering.time=datavr.time AND datasteering.idsession=datavr.idsession
  WHERE datasteering.idsession=$1;"
  session_data <- dbGetQuery(connection, query,c(session_id))
  
 # session_data_filtered <- session_data %>%
   # filter(steering>-1001,accelerator>-1001)
  

  
  #session_data_filtered$accelerator = session_data_filtered$accelerator/1000
  #session_data_filtered$brake = session_data_filtered$brake/1000
  session_data_filtered <- session_data %>% mutate_each_(funs(normalizar(.) %>% as.vector), 
                                                vars=c("accelerator","steering", "brake"))
  session_data_filtered <- session_data_filtered %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                               vars=c("positionx","positiony", "positionz", "rotationx", "rotationy", "rotationz"))
  
  session_data_filtered$longitudinal = (-session_data_filtered$accelerator+1)-(-session_data_filtered$brake+1)
  session_data_filtered$longitudinal = session_data_filtered$longitudinal/2
  
  session_data_filtered$steering_diff = predict(sm.spline(session_data_filtered$time, session_data_filtered$steering), session_data_filtered$time, 1)
  
  return(session_data_filtered)
}

get_speed_pos_data <- function(connection,session_id){
  query <- 'SELECT * FROM public."WorldSpeed" WHERE idSession=$1;'
  session_data <- dbGetQuery(connection, query,c(session_id))
   
  return(session_data)
}

get_car_data <- function(connection,session_id){
  query <- 'SELECT * FROM public."DistanceBrakeThrottleRpmSpeed" WHERE idSession=$1;'
  session_data <- dbGetQuery(connection, query,c(session_id))
  
  return(session_data)
}

get_car_full_data <- function(connection,session_id){
  query <- 'SELECT * FROM public."FullCarData" JOIN public."WorldSpeed" USING ("time", idsession, speed) 
  JOIN public."LapNumber1" USING ("time", idsession) WHERE idSession=$1;'
  session_car_data <- dbGetQuery(connection, query,c(session_id))
  
  return(session_car_data)
}

get_car_time_data <- function(connection,session_id){
  query <- 'SELECT * FROM public."FullCarData" 
  JOIN public."LapNumber1" USING ("time", idsession) WHERE idSession=$1;'
  session_car_data <- dbGetQuery(connection, query,c(session_id))
  
  return(session_car_data)
}

get_car_full_data_two_sessions <- function(connection,session_id1, session_id2){
  query <- 'SELECT * FROM public."FullCarData" JOIN public."WorldSpeed" USING ("time", idsession, speed) 
  JOIN public."LapNumber1" USING ("time", idsession) WHERE idSession=$1 OR idSession=$2;'
  session_car_data <- dbGetQuery(connection, query,c(session_id1, session_id2))
  
  return(session_car_data)
}

get_sector_data <- function(car_data, begin_sector, end_sector ){
  session_car_data_sector <- subset(car_data, car_data$distance > begin_sector & distance <= end_sector)
  return(session_car_data_sector)
}


connect_db()