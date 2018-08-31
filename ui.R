library(shiny)
library(leaflet)
ui <- fluidPage(
  # Application title
  titlePanel("Crime Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      #input crime date
      dateInput("date1", "Crime Date Begin:",value="2017-01-01",min="2017-01-01",max="2017-12-08"),
      dateInput("date2", "Crime Date End:",value="2017-12-08",min="2017-01-01",max="2017-12-08"),
      hr(),
      #select a crime time
      selectInput("time","Choose a crime time:",choices = c("Day"="DAY","Evening"="EVENING","Midnight"="MIDNIGHT"),selected="DAY"),
      hr(),
      fluidRow(
        #select crime method
        column(5, 
               checkboxGroupInput("method","Choose method:",choices = c("Gun"="GUN", "Knife"="KNIFE","Other Method"="OTHERS"))
               
        ),
        #select crime offense type
        column(7,
               checkboxGroupInput("offense","Choose offense:",
                                  choices = c("Arson"="ARSON", "Assault with Dangerous Weapon"="ASSAULT W/DANGEROUS WEAPON","Burglary"="BURGLARY",
                                              "Motor Vehicle Theft"="MOTOR VEHICLE THEFT","Homicide"="HOMICIDE","Robbery"="ROBBERY",
                                              "Sex Abuse"="SEX ABUSE","Theft from Vehicle"="THEFT F/AUTO","Theft from Other"="THEFT/OTHER"))
        )),
      p(
        class = "text-muted",
        h4("Note: The interactive parts of the plots and map may take",
              "a few seconds to react, we appreciate your patience!"
              
        )
  ),width=3),
    
    mainPanel(
      
      # Output: Tabset with table and plot
      tabsetPanel(type = "tabs",
                  tabPanel("Crime Descriptive Analysis", 
                           h4("Crime table in different time"),
                           tableOutput("table"),
                           h4("Crime counts diaplay(Please choose at least one method or offense)"),
                           verbatimTextOutput("text1"),
                           verbatimTextOutput("text2"),
                           verbatimTextOutput("text3"),
                           fluidRow(
                             column(5,
                                    h4("Crime pie chart by method"),
                                    plotOutput("plot3")
                             ),
                             column(7,
                                    h4("Crime pie chart by offense"),
                                    plotOutput("plot4")
                             )),
                           fluidRow(
                             #select crime method
                             column(5,
                                    h4("Crime histogram by method(Choose one or more methods)"),
                                    plotOutput("plot1")
                             ),
                             column(7,
                                    h4("Crime histogram by offense(Choose one or more offenses)"),
                                    plotOutput("plot2")
                             ))
                           
                           
                           ),
                  tabPanel("Crime Map", 
                           h5("Please choose ONLY one method or offense to calculate the number of crimes"),
                           leafletOutput("map",width = "100%", height = 600))
                  
      ),width=9
      
    )
  )
)