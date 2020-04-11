####################### 1. define Package to install #######################
library(rsconnect)
library(shiny)
library(dplyr)
library(tidyverse)
library(sf)
library(tmap)
library(plotly)
library(shiny)
library(stringr)
library(ggplot2)
library(ggridges)
library(htmlwidgets)
library(leaflet)
library(forcats)
library(shinydashboard)
library(waiter)
library(highcharter)
library(shinycssloaders)

################### 1.1 CSS ##########################
css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'Loading ...'; }
}
"
######################### 2. define dashboard UI ##########################

#~~~~~~~~~~~~~~~~~~~~~~~~~ 2.1 define dashboard elemets #~~~~~~~~~~~~~~~~~~~~~~~~~ 
header <- dashboardHeader(title = "Rain and Shiny Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Home Page",
      tabName = 'homepage',
      icon = icon("home")
      ),
    menuItem(
      "Climate Choropleth Map",
      tabName = "dashboard1",
      icon = icon("map")
    ),
    menuItem(
      "Temperature Radials",
      tabName = "dashboard5",
      icon = icon("dashboard")
    ),
    menuItem(
      "Correlation Plot",
      tabName = "dashboard4",
      icon = icon("dashboard")
    ),
    menuItem(
      "Violin Plot",
      tabName = "dashboard2",
      icon = icon("dashboard")
    ),
    menuItem(
      "Ridge Plot",
      tabName = "dashboard3",
      icon = icon("mountain")
    ),
    menuItem(
      "Trends",
      icon = icon("chart-line"),
      menuItem(
        "Temperature Trends",
         tabName = "dashboard6",
         icon = icon("dashboard")), 
      menuItem(
        "Tempertaure Trend Animated",
        tabName = "dashboard8",
        icon = icon("dashboard")
      )
    ),
    menuItem(
      "Calendar Heatmap",
      tabName = "dashboard7",
      icon = icon("calendar")
    )
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~  2.1.1 define dashboard body elements ~~~~~~~~~~~~~~~~~~~~~~~~~ 
homepage <- tabItem(tabName = "homepage",
                    fluidPage(fluidRow(
                      HTML('<center><img src="Rain_Shine.png" width="300"></center>')
                      
                    )),
                    fluidRow(
                        column(12, h2("Problem and Motivation")),
                        column(12, h5("The current reporting of Singapore's climate has always been primitive and thus it is challenging for viewers to obtain in-depth insights. In 2019, multiple news companies reported that Singapore is heating up twice as fast as the rest of the world. When combined with the island's constant high humidity, it could be life-threatening. Professor Matthias Roth of the department of geography at the National University of Singapore (NUS) attributed the rising temperatures to global warming and the Urban Heat Island (UHI) effect. However, there was no data given to back up their claims on Singapore's climate change.")),
                    ),
                    fluidRow(column(12, h5(
                    "Our team aims to present Singapore's climate data in more user-friendly and meaningful interpretation ways. Through Rain&Shine, an interactive and user-friendly visualization dashboard, that shows the distribution of the climate by Subzone, Region, and Singapore as a whole, we hope to provide Singaporeans with knowledge and in-depth insights into Singapore's Climate. Additionally, we want to identify the trends inherent within the weather data available and answer questions regarding the changes in Singapore's climate from available historical data.")
                    )))
dashboard1 <- tabItem(tabName = "dashboard1",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Map Plot"),
                        fluidRow(column(4, uiOutput("sYear")),
                                 column(4, uiOutput("sMonth"))),
                        fluidRow(column(4, leafletOutput("lxmap")),
                                 column(4, leafletOutput("lxmap2")))
                      ))

dashboard2 <- tabItem(tabName = "dashboard2",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Violin Plot"),
                        fluidRow(column(5, uiOutput("tYear1")),
                                 column(
                                   5,
                                   selectInput(
                                     inputId = "db2type",
                                     label = "Please Select:",
                                     choices = c("Rainfall", "Temperature"),
                                     selected = "Mean Rainfall"
                                   )
                                 )),
                        fluidRow(plotlyOutput(
                          "tanny1", width = "80%", height = "400px"
                        ))
                      ))

dashboard3 <- tabItem(tabName = "dashboard3",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Ridge Plot"),
                        fluidRow(uiOutput("tYear3"),
                                 fluidRow(
                                   column(6, plotOutput(
                                     "tanny2", width = "100%", height = "400px"
                                   )),
                                   column(6, plotOutput(
                                     "tanny3", width = "100%", height = "400px"
                                   ))
                                 ))
                      ))

dashboard4 <- tabItem(tabName = "dashboard4",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Correlation Plot"),
                        fluidRow(),
                        fluidRow(uiOutput("tYear")),
                        fluidRow(plotlyOutput("tanny4")),
                      ))

dashboard5 <- tabItem(tabName = "dashboard5",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Weathers Radials"),
                        fluidRow(column(
                          4,
                          sliderInput(
                            inputId = "hc_Year",
                            label = "Year:",
                            min = 2009,
                            max = 2019,
                            value = 2009,
                            step = 1,
                            sep = "",
                            animate = animationOptions(interval = 5000,
                                                       loop = FALSE)
                          )
                        ),
                        column(4, uiOutput('hcRegion'))),
                        fluidRow(column(
                          6,
                          highchartOutput("hc", width = "100%", height = "400px")
                        ))
                      ))

dashboard6 <- tabItem(tabName = "dashboard6",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Singapore Temperature Change (1982-2019)"),
                        fluidRow(withSpinner(
                          highchartOutput("hc2", width = "80%", height = "550px")
                        ))
                      ))

dashboard7 <- tabItem(tabName = "dashboard7",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        fluidRow(column(4, uiOutput("my_measure")),
                                 column(4, uiOutput("my_calendar"))),
                        fluidRow(plotOutput("my1"))
                      ))

dashboard8 <- tabItem(tabName = "dashboard8",
                      fluidPage(tags$style(type = "text/css", css),
                                fluidRow(plotOutput("my2"))))
#~~~~~~~~~~~~~~~~~~~~~~~~~ 2.1.2 Fill in dashboard elements ~~~~~~~~~~~~~~~~~~~~~~~~~ 
body <- dashboardBody(
  use_waiter(),
  waiter_show_on_load(tagList(spin_fading_circles(),
                              "Loading ...")),
  tabItems(
    homepage,
    dashboard1,
    dashboard2,
    dashboard3,
    dashboard4,
    dashboard5,
    dashboard6,
    dashboard7,
    dashboard8
  )
)

ui <- dashboardPage(header, sidebar, body)

# ~~~~~~~~~~~~~~~~~~~~~~~~~  2.1.3 import attribute data ~~~~~~~~~~~~~~~~~~~~~~~~~ 
mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")


# mainDF <- read.csv("merged data/dataset.csv")
# mainDF$date <- as.Date(with(mainDF, paste(Year, Month, Day,sep="-")), "%Y-%b-%d")

#-------------- Non Maps ------------
# rainfall <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Region,SZ,Year,Month) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# 
# temperature <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
#   group_by(Region,SZ,Year,Month) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# 
# masterDF <- rainfall %>%
#   mutate(Month = fct_relevel(Month, 
#                              "Jan","Feb","Mar",
#                              "Apr","May","Jun",
#                              "Jul","Aug","Sep",
#                              "Oct","Nov","Dec"))
# masterDF$mean_temp = temperature$mean_temp
# masterDF <- masterDF %>%
#   mutate_at(vars(mean_temp,mean_rain), funs(round(., 1))) %>%
#   na.omit()
masterDF <- readRDS("shiny data/masterDF.RDS")
#-------------- Maps----------------
# rainfall <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Year,SZ, Month) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# 
# temperature <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
#   group_by(Year,SZ, Month) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# 
# masterDF2 <- rainfall %>%
#   mutate(Month = fct_relevel(Month, 
#                              "Jan","Feb","Mar",
#                              "Apr","May","Jun",
#                              "Jul","Aug","Sep",
#                              "Oct","Nov","Dec"))
# masterDF2$mean_temp = temperature$mean_temp
# masterDF2<- masterDF2 %>%
#   mutate_at(vars(mean_temp,mean_rain), funs(round(., 1))) 
masterDF2 <- readRDS("shiny data/masterDF2.RDS")

#--------------Weathers trends------------
# Mastertemp <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Temperature")) %>%
#   group_by(Year, Month) %>%
#   summarise(median = median(Value, na.rm = TRUE),
#             lower = min(Value, na.rm = TRUE),
#             upper = max(Value, na.rm = TRUE),
#             avg = mean(Value, na.rm =TRUE)) %>%
#   na.omit()
# Mastertemp$date = as.Date(with(Mastertemp, paste(Year,month.abb[Month], "01", sep=" ")), "%Y %b %d")
# smooth_vals <- predict(loess(median~Year,Mastertemp))
# Mastertemp$smooth_vals <- smooth_vals
# 
# Mastertemp <- Mastertemp %>%
#   mutate_at(c(3:6,8), funs(round(., 1)))
Mastertemp <- readRDS("shiny data/Mastertemp.RDS")

Year_min <- min(Mastertemp[, "Year"], na.rm = TRUE)
Year_max <- max(Mastertemp[, "Year"], na.rm = TRUE)

#--------------Weathers Radials------------
# Mastertemp2 <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Temperature")) %>%
#   select(Region,Year, date, Value) %>%
#   group_by(Year,Region,date) %>%
#   summarise(min_temperaturec = min(Value, na.rm = TRUE),
#             max_temperaturec = max(Value, na.rm = TRUE),
#             mean_temperaturec = mean(Value, na.rm = TRUE),
#             median_temperaturec = median(Value, na.rm = TRUE)) %>%
#   na.omit()
# 
# smooth_vals <- predict(loess(median_temperaturec ~Year,Mastertemp2))
# Mastertemp2$smooth_vals <- smooth_vals
# 
# Mastertemp2 <- Mastertemp2 %>%
#   mutate_at(4:8, funs(round(., 1)))

Mastertemp2 <- readRDS("shiny data/Mastertemp2.RDS")
#-------------- Calendar Heatmap ----------------
# dat <- mainDF %>%
#   select(Year, Month, Day, Measurement, Value) %>%
#   filter(str_detect(mainDF$Measurement, "Mean Temperature|Daily Rainfall Total")) %>%
#   filter(!is.na(Value))
# 
# dat$date <- as.Date(with(dat, paste(Year, Month, Day, sep = "-")), "%Y-%b-%d")
# 
# dat$Year <- as.numeric(as.POSIXlt(dat$date)$year + 1900)
# dat$Month <- as.numeric(as.POSIXlt(dat$date)$mon + 1)
# dat$monthf <-
#   factor(
#     dat$Month,
#     levels = as.character(1:12),
#     labels = c(
#       "Jan",
#       "Feb",
#       "Mar",
#       "Apr",
#       "May",
#       "Jun",
#       "Jul",
#       "Aug",
#       "Sep",
#       "Oct",
#       "Nov",
#       "Dec"
#     ),
#     ordered = TRUE
#   )
# dat$weekday = as.POSIXlt(dat$date)$wday
# dat$weekdayf <-
#   factor(
#     dat$weekday,
#     levels = rev(0:6),
#     labels = rev(c(
#       "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
#     )),
#     ordered = TRUE
#   )
# dat$yearmonth <- as.yearmon(dat$date)
# dat$yearmonthf <- factor(dat$yearmonth)
# dat$week <- as.numeric(format(dat$date, "%W"))
# dat <- ddply(dat, .(yearmonthf), transform, monthweek = 1 + week - min(week))
# 
# smooth_vals = predict(loess(Value ~ Year, dat))
# dat$smooth_vals <- smooth_vals

dat <- readRDS("shiny data/dat.RDS")

######################### 3. define input output ##########################
server <- function(input, output, session) {
  #----------------------------------------dashboard 8 Animation -------------------------------
  output$my2 <- renderImage({
    list(src = "temperature_trend.gif",
         contentType = 'image/gif'
    )}, deleteFile = FALSE)
  #----------------------------------------dashboard 7 Calendar Heatmap -------------------------------
  output$my_calendar <- renderUI({

    sliderInput(
      inputId = "calendaryear",
      label = "Select time period: ",
      min = Year_min,
      max = Year_max,
      value = c(Year_min, Year_min + 3),
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  output$my_measure <- renderUI({
    selectInput(
      inputId = "my_measure",
      label = "Select to view",
      choices = c('Daily Rainfall Total (mm)', "Mean Temperature (°C)")
    )
  })
  output$my1 <- renderPlot({
    if (input$my_measure == "Daily Rainfall Total (mm)") {
      dat %>% filter(Measurement == input$my_measure) %>% 
        filter(Year <= input$calendaryear[2]) %>% 
        filter(Year >= input$calendaryear[1]) %>% 
        ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + 
        facet_grid(Year ~ monthf) + 
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(title = "Heatmap Across the Years", fill = input$my_measure) + 
        xlab("Week of Month") + ylab("")
    }
    else {
      dat %>% filter(Measurement == "Mean Temperature (Â°C)") %>% 
        filter(Year <= input$calendaryear[2]) %>% 
        filter(Year >= input$calendaryear[1]) %>% 
        ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + 
        facet_grid(Year ~ monthf) + 
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(title = "Heatmap Across the Years", fill = input$my_measure) + 
        xlab("Week of Month") + ylab("")
    }
  })
  #----------------------------------------dashboard 6 Climate Trend ---------------------------------------
  output$hc2 <- renderHighchart({
    x <- c("Max: ", "Median: ", "Min: ", "Predict: ")
    y <-
      sprintf("{point.%s}", c("upper", "median", "lower", "smooth_vals"))
    tltip <- tooltip_table(x, y)
    
    median_tmp <- mainDF %>%
      filter(str_detect(mainDF$Measurement, "Temperature"))
    
    med <- median(median_tmp$Value, na.rm = TRUE)
    low <- min(median_tmp$Value, na.rm = TRUE)
    high = max(median_tmp$Value, na.rm = TRUE)
    
    hchart(Mastertemp,
           type = "columnrange",
           hcaes(
             x = date,
             low = lower,
             high = upper,
             color = smooth_vals
           )) %>%
      hc_yAxis(
        tickPositions = c(low -5, med, high +5),
        gridLineColor = "#000000",
        labels = list(format = "{value} C", useHTML = TRUE)
      ) %>%
      hc_add_series(
        data = Mastertemp,
        type = "line",
        hcaes(x = date, y = smooth_vals),
        color = "#B71C1C"
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = as.character(tags$small("{point.x: %Y %b}")),
        pointFormat = tltip
      )
  })
  
  #----------------------------------------dashboard 5 Climate Radials ---------------------------------------
  output$hcRegion <- renderUI({
    tmp <- Mastertemp2 %>%
      filter(Year == as.numeric(input$hc_Year))
    selectInput(
      inputId = "hc_Region",
      label = "Select Region:",
      choices = unique(tmp$Region),
      selected = unique(tmp$Region)[1]
    )
  })
  
  
  output$hc <- renderHighchart({
    x <- c("Min", "Mean", "Max")
    y <- sprintf("{point.%s}", c("min_temperaturec", "mean_temperaturec", "max_temperaturec"))
    tltip <- tooltip_table(x, y)
    
    Mastertemp3 <- Mastertemp2 %>%
      filter(Region == as.character(input$hc_Region)) %>%
      filter(Year == as.numeric(input$hc_Year))
    
    hchart(Mastertemp3, type = "columnrange",
           hcaes(x = date, low = min_temperaturec, high = max_temperaturec,
                 color = mean_temperaturec)) %>% 
      hc_chart(polar = TRUE) %>%
      hc_yAxis( max = 40, min = 15, labels = list(format = "{value} C"),
                showFirstLabel = FALSE) %>% 
      hc_xAxis(
        title = list(text = ""), gridLineWidth = 0.5,
        labels = list(format = "{value: %b}")) %>% 
      hc_tooltip(useHTML = TRUE, pointFormat = tltip,
                 headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")))
  })
  
  #----------------------------------------dashboard 4 Correlation Plot---------------------------------------
  output$tYear <- renderUI({
    sliderInput(
      inputId = "YearTanny4",
      label = "Year:",
      min = Year_min,
      max = Year_max,
      value = Year_min,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  tanny4 <- reactive({
    masterDF[masterDF$Year ==  as.numeric(input$YearTanny4),]
  })
  
  output$tanny4 <- renderPlotly({
    scatterPlot <-
      ggplot(tanny4(),
             aes(
               x = mean_rain,
               y = mean_temp,
               color = as.factor(Region),
               text = paste(
                 "Mean Rain Precipitation: ",
                 mean_rain,
                 "mm",
                 "<br>Mean Temperature: ",
                 mean_temp,
                 "(\u00B0C)" ,
                 "<br>Month: ",
                 Month,
                 "<br>Region: ",
                 Region
               )
             )) +
      geom_point(alpha = 0.8) +
      stat_smooth(method = "lm", col = "black", size = 0.7,
                  fill = "gray60", alpha = 0.2) +
      theme(legend.position = "none",
            legend.title = element_blank()) +
      labs(y = "Temperature (\u00B0C)", x = "Rain Precipitation (mm)")
    
    db4scatter <- ggplotly(scatterPlot, tooltip = "text")
    
    raindensity <-
      ggplot(tanny4(), aes(mean_rain)) +
      geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
      geom_density(alpha = .5, fill = '#C7E4EA') +
      theme(legend.position = "none")
    
    db4rain <- ggplotly(raindensity)
    
    tempdensity <-
      ggplot(tanny4(), aes(mean_temp)) +
      geom_histogram(aes(y = ..density..,),
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = .5, fill = '#E69F00') +
      theme(legend.position = "none") +
      coord_flip()
    
    db4temp <- ggplotly(tempdensity)
    
    blankPlot <- ggplot() +
      theme_void()
    
    blankPlot <- ggplotly(blankPlot)
    
    db4 <-
      subplot(
        db4rain,
        blankPlot,
        db4scatter,
        db4temp,
        nrows = 2,
        widths = c(0.7, 0.3),
        heights = c(0.3, 0.7),
        shareX = TRUE,
        shareY = TRUE
      )
  })
  #----------------------------------------dashboard 3 Ridge Plot---------------------------------------
  output$tYear3 <- renderUI({
    sliderInput(
      inputId = "YearTanny3",
      label = "Year:",
      min = 2009,
      max = Year_max,
      value = 2009,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  output$tanny2 <- renderPlot({
    masterDF %>%
      filter(Year == as.numeric(input$YearTanny3)) %>%
      ggplot(aes(x = mean_temp, y = Month, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Temperature (\u00B0C)", option = "B") +
      labs(title = 'Temperatures',
           subtitle = 'Mean temperatures (Celcius) by Month') +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(axis.title.y = element_blank()) + xlim(min(masterDF$mean_temp, na.rm = TRUE) - 1,
                                                   max(masterDF$mean_temp, na.rm = TRUE) + 1)
  })
  
  output$tanny3 <- renderPlot({
    masterDF %>%
      filter(Year == as.numeric(input$YearTanny3)) %>%
      ggplot(aes(x = mean_rain, y = factor(Month), fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Precipitation (mm)", option = "D") +
      labs(title = 'Rainfal',
           subtitle = 'Mean Rainfall Precipitation (mm) by Month') +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(axis.title.y = element_blank())
  })
  
  #----------------------------------------dashboard 2 Voilin Plot  ---------------------------------------
  output$tYear1 <- renderUI({
    sliderInput(
      inputId = "YearTanny1",
      label = "Year:",
      min = 2009,
      max = Year_max,
      value = 2009,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  output$tanny1 <- renderPlotly({
    rain <- ggplot(masterDF %>%
                     filter(Year == as.numeric(input$YearTanny1)),
                   aes(factor(Month),
                       if (input$db2type == "Rainfall") {
                         mean_rain
                       } else{
                         mean_temp
                       }
                   )) +
      geom_violin(color = "purple",
                  add = "boxplot",
                  fill = "purple",
                  alpha= 0.5) +
      geom_boxplot(width = 0.1,
                   fill = "grey",
                   alpha = 1) +
      theme(legend.position = "none") +
      xlab("") +
      ylab(if (input$db2type == "Rainfall") {
        "Average Rainfall ( mm )"
      } else{
        "Average temperature (\u00B0C)"
      })
    
    rain <- ggplotly(rain, tooltip = "text")
  })
  #----------------------------------------dashboard 1 Choropleth Map ----------------------------------------
  # render filters in UI 
  output$sYear <- renderUI({
    sliderInput(
      inputId = "YearLX",
      label = "Year:",
      min = Year_min,
      max = Year_max,
      value = Year_min,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  output$sMonth <- renderUI({
    selectInput(
      inputId = "MonthLX",
      label = "Month:",
      choices = c("Jan","Feb","Mar",
                  "Apr","May","Jun",
                  "Jul","Aug","Sep",
                  "Oct","Nov","Dec"),
      selected = "Jan"
    )
  })
  
  tmp<-reactive({
    tmp2 <- masterDF2 %>%
      filter(Year == as.numeric(input$YearLX)) %>%
      filter(Month == as.character(input$MonthLX))
    
    tmp2 <- left_join(mpsz, tmp2,
                      by = c("SUBZONE_N" = "SZ")) %>%
      st_transform(4326)
  })
  
  # show rain map
  output$lxmap = renderLeaflet({
    tm <- tm_shape(tmp())+
      tm_fill("mean_rain",
              style = "quantile",
              palette = "Blues") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  # show temp map
  output$lxmap2 = renderLeaflet({
    tm <- tm_shape(tmp())+
      tm_fill("mean_temp",
              style = "quantile",
              palette = "Oranges") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  waiter_hide()
}
######################### 4. Finish app ##########################
shinyApp(ui,server)