# Load required libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(mosaic)
library(corrplot)

# Read in dataset, name it df
df <- read.csv("day.csv")

# Drop variables, rename variables in df
df <- df%>% 
    select(-c("instant", "yr", "mnth", "weekday")) %>% 
    rename(Date = dteday, Season = season, Holiday = holiday, Workday = workingday,
           Weather = weathersit, Temperature = temp, Feeling_temperature = atemp, 
           Humidity = hum, Windspeed = windspeed, Nonmember = casual, 
           Member = registered, All_users = cnt)

# Modify class of variables appropriately
df$Date <- as.Date(df$Date)
df$Season <- factor(df$Season)
df$Holiday <- factor(df$Holiday)
df$Workday <- factor(df$Workday)
df$Weather <- factor(df$Weather)

# Treat factor variables' levels appropriately
levels(df$Season)[levels(df$Season) == "1"] <- "Winter"
levels(df$Season)[levels(df$Season) == "2"] <- "Spring"
levels(df$Season)[levels(df$Season) == "3"] <- "Summer"
levels(df$Season)[levels(df$Season) == "4"] <- "Fall"

levels(df$Holiday)[levels(df$Holiday) == "1"] <- "Yes"
levels(df$Holiday)[levels(df$Holiday) == "0"] <- "No"

levels(df$Workday)[levels(df$Workday) == "1"] <- "Yes"
levels(df$Workday)[levels(df$Workday) == "0"] <- "No"

levels(df$Weather)[levels(df$Weather) == "1"] <- "Clear/ Few clouds"
levels(df$Weather)[levels(df$Weather) == "2"] <- "Mist/ Cloudy"
levels(df$Weather)[levels(df$Weather) == "3"] <- "Light snow/ Light rain"
levels(df$Weather)[levels(df$Weather) == "4"] <- "Heavy rain/ Thunderstorm/ Snow"

# Define UI
shinyUI(
    fluidPage(
        
        # Title of the app
        titlePanel("Exploring bike rental in Washington D.C. (2011 & 2012), by Rio Zhao"),
        
        sidebarLayout(
            
            # Create appropriate sidebar panels
            sidebarPanel(
                
                selectInput(inputId = "select1", label = "Boxplot X-axis", 
                            choices = c("Holiday" = "Holiday", "Workday" = "Workday"), 
                            selected = "Holiday"),
                
                selectInput(inputId = "select2", label = "Boxplot Y-axis:", 
                            c(`All users` = "All_users", "Member" = "Member", "Nonmember" = "Nonmember"),
                            selected = "All_users"), 
                
                checkboxInput(inputId = "boxplotcheck", "Show boxplot summary", TRUE),
                
                checkboxGroupInput(inputId = "checkgroup1", label = "Membership", 
                                   choices = c(`All users` = "All_users", "Member" = "Member", "Nonmember" = "Nonmember"),
                                   selected = "All_users"),
                
                checkboxGroupInput(inputId = "checkgroup2", label = "Weather variables (normalized)", 
                                   choices = c("Temperature" = "Temperature", `Feeling temperature` = "Feeling_temperature", 
                                               "Humidity" = "Humidity", `Wind speed` = "Windspeed"), 
                                   selected = "Temperature"),
                
                dateRangeInput(inputId = "dates", label = "Date range", min = "2011-01-01", max = "2012-12-31",
                               start = "2011-01-01", end = "2012-12-31"), 
                
                checkboxInput(inputId = "fit", label = "Add a best fit line to scatterplot", TRUE), 
                
                br(), 
                
                h5("Built with", 
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                   "by",
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"), 
                   " for ", 
                   img(src = "https://www.kumc.edu/Images/graphics/logos/KUMC_scr_UnitHorz_2color.png", height = "30px"))
                
                
            ),
            
            # Create appropriate main panel tabs
            mainPanel(
                tabsetPanel(type = "tabs",
                            
                            tabPanel(title = "Data Table", h3("Data Table"), br(), span(textOutput(outputId = "tabletext1"), 
                            		 style = "color:red"),
                                     br(), textOutput(outputId = "tabletext2"), br(),
                                     DT::dataTableOutput(outputId = "datatable")), 
                            
                            tabPanel(title = "Data Summary", h3("Summary Statistics of dataset"), br(), 
                                     verbatimTextOutput("summary")), 
                            
                            tabPanel(title = "Boxplot", h3("Boxplot"), textOutput(outputId = "boxplottext1"), 
                                     plotOutput(outputId = "boxplot1"), verbatimTextOutput(outputId = "boxplotsummary"), 
                                     br(), textOutput(outputId = "boxplottext2"), br(), plotOutput(outputId = "boxplot2")), 
                            
                            tabPanel(title = "Independent t-test", h3("Independent t-test"),  
                                     textOutput(outputId = "ttesttext1"),
                                     br(), span(textOutput(outputId = "ttesttext2"), style = "color:red") ,br(), 
                                     textOutput(outputId = "ttesttext3"), br(), verbatimTextOutput(outputId = "ttestoutput"), 
                                     br(), textOutput(outputId = "ttesttext4")),
                            
                            tabPanel(title = "Time-series plot", h3("Time-series plot"), 
                                     textOutput(outputId = "timeseriestext"), br(), plotOutput(outputId = "timeseries")), 
                            
                            tabPanel(title = "Barplot", h3("Barplot"), textOutput(outputId = "barplottext1"), br(), 
                                     plotOutput(outputId = "barplot1"), br(), br(), textOutput(outputId = "barplottext2"), br(),
                                     plotOutput(outputId = "barplot2")),
                            
                            tabPanel(title = "Correlation Plot", h3("Correlation Plot"), textOutput(outputId = "cortext1"), 
                                     br(), span(textOutput(outputId = "cortext2"), style = "color:red"), br(),
                                     plotOutput(outputId = "corplot")),
                            
                            tabPanel(title = "Regression", h3("Simple Linear Regression Model and its Summary Statistics"), 
                                     textOutput(outputId = "regressiontext1"), br(), 
                                     span(textOutput(outputId = "regressiontext2"), style = "color:red"),
                                     br(), textOutput(outputId = "regressiontext3"), br(), textOutput(outputId = "correlation"), 
                                     plotOutput(outputId = "regression"), br(), verbatimTextOutput(outputId = "lmoutput"))
                )
            )
        )
    )
)
