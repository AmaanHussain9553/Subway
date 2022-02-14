#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#List of all libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(rlang)
library(plyr)
library(dplyr)



# Reading in the main file and breaking it into chunks of 4.8 MB
# Only run this code once
#my_file <- read.csv("CTA_Data_Total.csv")
#grps <- (split(my_file, (seq(nrow(my_file))-1) %/% 95000))
#for (i in seq_along(grps)) {
#  write.csv(grps[[i]], paste0("CTA_Data", i, ".csv"))
#}

#delete the main CTA_Data file and bind all the 
#other CSV files together
temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, read.csv)
allData <- rbind.fill(allData2)







#converting data to internal format 
allData$newDate <- as.Date(allData$date, "%m/%d/%Y")

#creating new column for month and day of the week
allData$month <- floor_date(allData$newDate, "month")
allData$day <- weekdays(as.Date(allData$newDate))

#debug print to ensure all data is loaded well
#print(allData)

#removing the old date column
allData$Date <- NULL

#Creating a new grouped data table for monthly 
#aggregated stats
monthlyGroup <- allData %>%                        
  group_by(month) %>% 
  dplyr::summarize(No_of_Rides = mean(rides), station_name = stationname) %>% 
  as.data.frame()

#Creating a new grouped data table for 
#aggregated stats based on day of the week
weeklyGroup <- allData %>%                        
  group_by(day) %>% 
  dplyr::summarize(No_of_Rides = mean(rides), station_name = stationname, date = newDate) %>% 
  as.data.frame()

#debug print statements to look at new grouped data tables
#print(monthlyGroup)
#print(weeklyGroup)

#list to hold the options for years and stations
years<-c(2001:2021)
location<-unique(allData$stationname)
locationOrdered <- sort(location)





# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     selectInput("Year", "Select the year for UIC-Halsted", years, selected = 2021),
                     selectInput("Loc", "Select location to display", locationOrdered, selected = "UIC-Halsted")
  ),
  dashboardBody(
    fluidRow(
      #this column holds the 3 bar charts based on requirements part 1
      column(6,
             fluidRow(
               box(title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist1", height = 300)
               )
             ),
             
             
             fluidRow(
               box(title = "Monthly data (mean) of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist2", height = 300)
               )
             ),
             
             fluidRow(
               box(title = "Number of rides based on days of the week", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist3", height = 300)
               )
             ),
             
      ),
      #this column holds the 3 data tables based on requirements part 1
      column(6,
             fluidRow(
               box( title = "Daily data of Rides per dayr", solidHeader = TRUE, status = "primary", width = 12,
                    dataTableOutput("tab1", height = 300)
               )
             ), 
             fluidRow(
               box( title = "Monthly data (mean) of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                    dataTableOutput("tab2", height = 300)
               )
             ),
             fluidRow(
               box( title = "Number of rides based on days of the week", solidHeader = TRUE, status = "primary", width = 12,
                    dataTableOutput("tab3", height = 300)
               )
             )
      )
    )
  )
)

server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 14))
  #creating reactive objects that can look at data yearly, monthly, and based on day of the week
  #it is reactive so that it may change as the sidebar options change
  justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & allData$stationname == input$Loc)})
  monthlyReactive <- reactive({subset(monthlyGroup, year(monthlyGroup$month) == input$Year & monthlyGroup$station_name == input$Loc)})
  weeklyReactive <- reactive({subset(weeklyGroup, year(weeklyGroup$date) == input$Year & weeklyGroup$station_name == input$Loc)})
  
    #daily bar plot based on the year
    output$hist1 <- renderPlot({
      justOneYear = justOneYearReactive()
      ggplot(justOneYear, aes(x=newDate, y=rides)) +
        labs(x=paste("Day in", input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) + scale_y_continuous()
    })
    
    #monthly bar plot based on the year
    output$hist2 <- renderPlot({
      monthly <- monthlyReactive()
      ggplot(monthly, aes(x=month, y=No_of_Rides)) +
        labs(x=paste("Month in", input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) + scale_y_continuous()
    })
    
    #day of the week bar plot based on the year
    output$hist3 <- renderPlot({
      weekly <- weeklyReactive()
      ggplot(weekly, aes(x=day, y=No_of_Rides)) + labs(x=paste("Month in", input$Year), y = "No. Rides") + 
        geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) + scale_y_continuous()
    })
    
    #daily data table based on the year
    output$tab1 <- DT::renderDataTable(
      DT::datatable({
        justOneYear <- justOneYearReactive()
        
        as.data.frame(justOneYear[, c(Date = "newDate", Rides = "rides")])
      }, 
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
      ), rownames = FALSE 
      )
    )
    
    #monthly data table based on the year
    output$tab2 <- DT::renderDataTable(
      DT::datatable({
        monthly <- monthlyReactive()
        
        as.data.frame(monthly[, c(Month = "month",Rides = "No_of_Rides")])
      }, 
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
      ), rownames = FALSE 
      )
    )
    
    #day of the week data table based on the year
    output$tab3 <- DT::renderDataTable(
      DT::datatable({
        weekly <- weeklyReactive()
        
        as.data.frame(weekly[, c(Day_of_week = "day",Rides = "No_of_Rides")])
      }, 
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
      ), rownames = FALSE 
      )
    )
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
