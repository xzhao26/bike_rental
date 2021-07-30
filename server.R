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

# Define server
shinyServer(function(input, output) {
    
    # Create table for the dataset
    output$datatable <- DT::renderDataTable({
        df
    })
    
    output$tabletext1 <- renderText({
        "Note: Not all variables from the original dataset are included in this project."
    })
    
    output$tabletext2 <- renderText({
        "Click the button shown below to see the data by your specified number of rows."
    })
    
    # Create summary for dataset
    output$summary <- renderPrint({
        summary(df)
    })
    
    # Create interactive boxplots of continuous variables
    output$boxplot1 <- renderPlot({
        a <- df %>% pull(input$select2)
        boxplot(a, col = "steelblue1", 
                main = paste0("Boxplot: bike rental count for ", input$select2), 
                ylab = "Rental count")
    })
    
    # Create interactive summary of continuous variables
    output$boxplotsummary <- renderPrint({
        a <- df %>% pull(input$select2)
        
        # Show summary if checkbox is checked
        if(input$boxplotcheck){fav_stats(a)}
    })
    
    # Create interactive boxplots of categorical variable against continuous variables
    output$boxplot2 <- renderPlot({
        m <- paste0(input$select2, "~", input$select1)
        boxplot(as.formula(m), data = df, col = "steelblue1", 
                main = paste0("Boxplot: bike rental for ", input$select2, " in ", input$select1), 
                ylab = paste0(input$select2, " rental count"))
    })
    
    output$boxplottext1 <- renderText({
        paste0("The boxplot below illustrates bike rental count for ", input$select2, ".")
    })
    
    output$boxplottext2 <- renderText({
        paste0("The boxplot below illustrates bike rental count for ", input$select2, " in ", input$select1, ".")
    })
    
    # Perform independent t-test, and print its result
    output$ttestoutput <- renderPrint({
        continuous <- df %>% pull(input$select2)
        categorical <- df %>% pull(input$select1)
        print(t.test(continuous ~ categorical, data=df))
    })
    
    output$ttesttext1 <- renderText({
        "An independent samples t-test compares the means of two independent groups in order to determine whether 
        there is statistical evidence that the associated population means are significantly different. In the following
        t-test, the alpha level is set at 0.05. The t-test has a null hypothesis that the means of two groups are equal 
        and an alternative hypothesis that the means of two groups are not equal. If the p-value of the test is less than 
        0.05, we reject the null hypothesis. If the p-value is equal or larger than 0.05, we fail to reject the null hypothesis."
    })
    
    output$ttesttext2 <- renderText({
        "Tips: Please interact with the Boxplot X-axis and Boxplot Y-axis drop-down menus on the left to see the t-test 
        results."
    })
    
    output$ttesttext3 <- renderText({
        paste0("The following t-test summary takes ", input$select1, " as the categorical variable and bike rental count for ", 
               input$select2, " as the continuous variable.")
    })
    
    output$ttesttext4 <- renderText({
        continuous <- df %>% pull(input$select2)
        categorical <- df %>% pull(input$select1)
        pvalue <- round(t.test(continuous ~ categorical, data=df)$p.value, 4)
        paste0("The p-value of the above t-test is ", pvalue, ".")
    })
    
    output$timeseriestext <- renderText({
        "The time-series plot below visualizes bike rental count among different types of users from years 2011 to 2012."
    })
    
    # Create time series plot, showing 3 continuous variables in one line plot
    output$timeseries <- renderPlot({
        
        df.variable <- df %>%
            select(Date, Member, Nonmember, All_users) %>%
            gather(key = Membership, value = value, -Date)
        
        df.variable.long <- df.variable %>% 
            select(Date, Membership, value) %>% 
            group_by(Date, Membership) %>% 
            summarise(`Rental count` = sum(value))
        
        d <- reactive({
            df.variable.long %>% 
                filter(Membership %in% input$checkgroup1, 
                       Date >= input$dates[1] & Date <= input$dates[2])
        })
        
        ggplot(d(), aes(x = Date, y = `Rental count`, color = Membership)) + geom_line() +
            ggtitle("Bike rental from 2011 to 2012") + theme_bw() + 
            theme(axis.text = element_text(size = 13), 
                  axis.title = element_text(size = 18), 
                  plot.title = element_text(size = 22, face = "bold", hjust = 0.5, 
                                            margin = margin(t = 0, r = 0, b = 20, l = 0)), 
                  legend.text = element_text(size = 13), 
                  legend.title = element_text(size = 18)) + 
            scale_y_continuous(breaks = seq(0,9000,1000)) + 
            scale_x_date(date_breaks = "4 month")
    })
    
    output$barplottext1 <- renderText({
        "The barplot below visualizes bike rental count for different groups of users under different weather conditions."
    })
    
    # Create barplot of 3 continuous variables, use their values on Y-axis, a categorical variable (weather) on X-axis, 
    # colored by the continuous variables themselves
    output$barplot1 <- renderPlot({
        
        df.variable <- df %>%
            select(Member, Nonmember, All_users, Weather) %>%
            gather(key = Membership, value = value, -Weather)
        
        df.variable.long <- df.variable %>% 
            select(Membership, value, Weather) %>% 
            group_by(Membership, Weather) %>% 
            summarise(`Rental count` = sum(value))
        
        d <- reactive({
            df.variable.long %>% 
                filter(Membership %in% input$checkgroup1)
        })
        
        ggplot(d(), aes(x = Weather, y = `Rental count`, fill = Membership)) + 
            geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::comma) + 
            ggtitle("Bike rental count under various weather conditions") +
            theme_bw() + theme(axis.text = element_text(size = 12), 
                               axis.title = element_text(size = 18), 
                               plot.title = element_text(size = 22, face = "bold", hjust = 0.5, 
                                                         margin = margin(t = 0, r = 0, b = 20, l = 0)), 
                               legend.text = element_text(size = 11), 
                               legend.title = element_text(size = 16)) + 
            geom_text(aes(label = `Rental count`), position = position_dodge(width = 1), vjust = -0.25)
    })
    
    output$barplottext2 <- renderText({
        "The barplot below visualizes bike rental count for different groups of users in different seasons."
    })
    
    # Create barplot of 3 continuous variables, use their values on Y-axis, a categorical variable (season) on X-axis, 
    # colored by the continuous variables themselves
    output$barplot2 <- renderPlot({
        df.variable <- df %>%
            select(Member, Nonmember, All_users, Season) %>%
            gather(key = Membership, value = value, -Season)
        
        df.variable.long <- df.variable %>% 
            select(Membership, value, Season) %>% 
            group_by(Membership, Season) %>% 
            summarise(`Rental count` = sum(value))
        
        d <- reactive({
            df.variable.long %>% 
                filter(Membership %in% input$checkgroup1)
        })
        
        ggplot(d(), aes(x = Season, y = `Rental count`, fill = Membership)) + 
            geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::comma) + 
            ggtitle("Bike rental count in different seasons") +
            theme_bw() + theme(axis.text = element_text(size = 12), 
                               axis.title = element_text(size = 18), 
                               plot.title = element_text(size = 22, face = "bold", hjust = 0.5, 
                                                         margin = margin(t = 0, r = 0, b = 20, l = 0)), 
                               legend.text = element_text(size = 11), 
                               legend.title = element_text(size = 16)) + 
            geom_text(aes(label = `Rental count`), position = position_dodge(width = 1), vjust = -0.25)
    })
    
    output$cortext1 <- renderText({
        "The following corrleation plot illustrates the degree of association between continuous variables in the dataset."
    })
    
    output$cortext2 <- renderText({
        "Tips: You may choose multiple continuous variables on the left to visualize their correlation. The exact degree of correlation 
        between two variables can be obtained in the Regression section of this project."
    })
    
    output$corplot <- renderPlot({
        d <- reactive({
            df <- df %>% select(input$checkgroup1, input$checkgroup2)
        })
        
        corrplot(cor(d()), type = "lower")
    })
    
    output$regressiontext1 <- renderText({
        "In this simple linear regression model, the independent variables are various weather conditions while the
        depedent variables are bike rental count for different groups of users."
    })
    
    output$regressiontext2 <- renderText({
        "Tips: A simple linear regression model considers only one independent variable and one depedent varaible. 
        Selecting more than one variable for each category on the left will not produce any results in plotting or model summary."
    })
    
    output$regressiontext3 <- renderText({
        paste0("The regression plot below takes ", input$checkgroup2, " as the independent variable and bike rental count for ", 
               input$checkgroup1, " as the depedent variable.")
    })
    
    # Calculate correlation between 2 continuous variables
    output$correlation <- renderText({
        r <- round( cor(df[, input$checkgroup2], df[, input$checkgroup1], use = "pairwise"), 4)
        paste0("Correlation between ", input$checkgroup2, " and bike rental count of ", input$checkgroup1, " = ", r, ".")
    })
    
    # Print regression model's summary
    output$lmoutput <- renderPrint({
        x <- df %>% pull(input$checkgroup2)
        y <- df %>% pull(input$checkgroup1)
        print(summary(lm(y ~ x, data = df)), digit = 3)
    })
    
    # Plot regression model
    output$regression <- renderPlot({
        x <- df %>% pull(input$checkgroup2)
        y <- df %>% pull(input$checkgroup1)
        plot(x, y, main = paste0("Regression plot of ", input$checkgroup2, " against bike rental count for ", 
                                 input$checkgroup1), xlab = paste0(input$checkgroup2, " (normalized)"), 
             ylab = paste0("Rental count of ", input$checkgroup1))
        
        # Add a best-fit line if checkbox is checked
        if(input$fit){abline(lm(y ~ x), col = "red", lwd = 2)}
    })
    
})
