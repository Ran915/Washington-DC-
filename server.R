library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(forcats)
library(scales)
# Read dataset file
crime <- read.csv("Crime_Incidents_in_2017.csv",header = TRUE)
# change data type to date
crime$REPORT_DAT <- as.Date(crime$REPORT_DAT)
# add a new column which converts the OFFENSE to factor
crime$category <- factor(crime$OFFENSE)
# assign each factor a color
pal <- colorFactor(topo.colors(9), crime$category)


shinyServer(function(input, output,session) {
  
  #show analysis table
  output$table <- renderTable({
    subset1 <- subset(crime,SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    my_tbl = addmargins(xtabs(~METHOD+OFFENSE,data=subset1))
    printed_tbl = as.data.frame.matrix(my_tbl)
    printed_tbl
  },include.rownames=TRUE)
  # show text of total number of crimes
  output$text1 <- renderText({
    if(length(input$method)==3|length(input$offense)==9){
      subset1 <- subset(crime,SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
      paste('The total number of crimes in the', tolower(input$time), 'from', input$date1, 'to', 
            input$date2, 'is', nrow(subset1))
    }
  })
  # show the text of crime numbers of different methods
  output$text2 <- renderText({
    subset2 <- subset(crime,SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    if(length(input$method)==1){
      paste('The number of crimes in the', tolower(input$time), 'using',
            tolower(input$method),'from',input$date1, 'to', input$date2, 'is', 
            nrow(subset(subset2,METHOD==input$method)))
    }
    
    else if(length(input$method)==2){
      paste('The number of crimes in the',tolower(input$time),'using',
            tolower(input$method[1]),'and',tolower(input$method[2]),'from',
            input$date1,'to',input$date2,'is',
            nrow(subset(subset2,METHOD==input$method[1]|METHOD==input$method[2])))
    }
  })
  # show the text of crime numbers of different offenses
  output$text3 <- renderText({
    subset2 <- subset(crime,SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    if(length(input$offense)==1){
      paste('The number of', tolower(input$offense),'crimes in the', tolower(input$time), 
            'from',input$date1, 'to', input$date2, 'is', 
            nrow(subset(subset2,OFFENSE==input$offense)))
    }
    
    else if(length(input$offense)==2){
      paste('The number of', tolower(input$offense[1]),'and',tolower(input$offense[2]),'crimes in the', tolower(input$time), 
            'from',input$date1, 'to', input$date2, 'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2])))
    }
    
    else if(length(input$offense)==3){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),'and',tolower(input$offense[3]),
            'crimes in the', tolower(input$time), 'from',input$date1, 'to', input$date2, 'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3])))
    }
    else if(length(input$offense)==4){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),',',tolower(input$offense[3]),
            'and',tolower(input$offense[4]),'crimes in the',tolower(input$time), 'from',input$date1,'to',input$date2,'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3]|OFFENSE==input$offense[4])))
    }
    else if(length(input$offense)==5){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),',',tolower(input$offense[3]),
            ',',tolower(input$offense[4]),'and',tolower(input$offense[5]),'crimes in the',
            tolower(input$time), 'from',input$date1,'to',input$date2,'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3]|
                          OFFENSE==input$offense[4]|OFFENSE==input$offense[5])))
                                                                                         
    }
    else if(length(input$offense)==6){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),',',tolower(input$offense[3]),
            ',',tolower(input$offense[4]),',',tolower(input$offense[5]),'and',tolower(input$offense[6]),'crimes in the',
            tolower(input$time), 'from',input$date1,'to',input$date2,'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3]|
                          OFFENSE==input$offense[4]|OFFENSE==input$offense[5]|OFFENSE==input$offense[6])))
                                                                                         
    }
    else if(length(input$offense)==7){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),',',tolower(input$offense[3]),
            ',',tolower(input$offense[4]),',',tolower(input$offense[5]),',',tolower(input$offense[6]),'and',
            tolower(input$offense[7]),'crimes in the',tolower(input$time), 'from',input$date1,'to',input$date2,'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3]|
                          OFFENSE==input$offense[4]|OFFENSE==input$offense[5]|OFFENSE==input$offense[6]|
                          OFFENSE==input$offense[7])))
      
    }
    else if(length(input$offense)==8){
      paste('The number of', tolower(input$offense[1]),',',tolower(input$offense[2]),',',tolower(input$offense[3]),
            ',',tolower(input$offense[4]),',',tolower(input$offense[5]),',',tolower(input$offense[6]),',',
            tolower(input$offense[7]),'and',tolower(input$offense[8]),'crimes in the',tolower(input$time), 'from',
            input$date1,'to',input$date2,'is', 
            nrow(subset(subset2,OFFENSE==input$offense[1]|OFFENSE==input$offense[2]|OFFENSE==input$offense[3]|
                          OFFENSE==input$offense[4]|OFFENSE==input$offense[5]|OFFENSE==input$offense[6]|
                          OFFENSE==input$offense[7]|OFFENSE==input$offense[8])))
      
    }
    
  })
  # show bar chart by different method
  output$plot3<- renderPlot({
    subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    my_tbl = as.data.frame(xtabs(~METHOD,data=subset3))
    ggplot(my_tbl,aes(x=1,weight=Freq,fill=METHOD))+geom_bar(position = "stack")+coord_polar(theta = "y")
  })
  # show bar chart by different offenses
  output$plot4<- renderPlot({
    subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    my_tbl = as.data.frame(xtabs(~OFFENSE,data=subset3))
    ggplot(my_tbl,aes(x=1,weight=Freq,fill=OFFENSE))+geom_bar(position = "stack")+coord_polar(theta = "y")
  })
  # show histogram by date in different methods
  output$plot1<- renderPlot({
    if(length(input$method)==3|length(input$method)==0){
      subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2,select=c('REPORT_DAT','SHIFT','METHOD'))
    }
    else
      subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2 &METHOD==input$method,select=c('REPORT_DAT','SHIFT','METHOD'))
    orderdata <- subset3[order(subset3$REPORT_DAT),]
    ggplot(orderdata,aes(x=REPORT_DAT,fill=METHOD))+geom_histogram(bins=12)
  })
  # show histogram by date in different offenses
  output$plot2<- renderPlot({
    if(length(input$offense)==9|length(input$offense)==0){
      subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2,select=c('REPORT_DAT','SHIFT','OFFENSE'))
    }
    else
      subset3 <- subset(crime,SHIFT==input$time& REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2 &OFFENSE==input$offense,select=c('REPORT_DAT','SHIFT','OFFENSE'))
    orderdata <- subset3[order(subset3$REPORT_DAT),]
    
    ggplot(orderdata,aes(x=REPORT_DAT,fill=OFFENSE))+geom_histogram(bins=12)
  })
  
  
  
  
  # create leaflet map
  output$map <- renderLeaflet({
    startset <- subset(crime,SHIFT=="DAY")
    pal1 <- colorFactor(topo.colors(8), startset$category)
    leaflet(startset) %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      addCircles(lng = startset$LONGITUDE,
                 lat = startset$LATITUDE, 
                 popup=paste(startset$REPORT_DAT,startset$SHIFT,startset$OFFENSE),
                 opacity=0.7
                 ,
                 color = ~pal1(category)) %>%
      addLegend("bottomleft",pal = pal1, values = ~category, opacity = 0.7)%>%
      setView(lng = -77.0365,
              lat = 38.8977,
              zoom = 12)
  })
  # create interactive filtered date
  filteredData <- reactive({
    if(length(input$method)==0&length(input$offense)==0){
      subset(crime, SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2)
    }
    else if(length(input$method)==0 & length(input$offense)!=0){
      subset(crime, SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2 & OFFENSE==input$offense)
    }
    else if(length(input$method)!=0 & length(input$offense)!=0){
      subset(crime, SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2 & METHOD==input$method & OFFENSE==input$offense)
    }
    else if(length(input$method)!=0 & length(input$offense)==0){
      subset(crime, SHIFT==input$time & REPORT_DAT>=input$date1 & REPORT_DAT<=input$date2 & METHOD==input$method)
    }
  })
  
  # observe interactive actions and change the shape of the map
  observe({
    leafletProxy("map",data=filteredData()) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      clearControls()%>%
      #clearMarkerClusters() %>%
      #clearMarkers() %>%
      addCircles(lng = ~LONGITUDE,
                 lat = ~LATITUDE, 
                 popup= ~paste(REPORT_DAT,METHOD,OFFENSE),
                 opacity=0.7,
                 color = ~pal(category))%>%
      addLegend("bottomleft",pal = pal, values = ~category, opacity = 0.7)
      
  })
  
  
  
})