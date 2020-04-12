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
#~~~~~~~~~~~~~~~2.1 define dashboard elemets #~~~~~~~~~~~~~~~~~
header <- dashboardHeader(title = "Rain and Shiny Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Home Page",
      tabName = 'homepage',
      icon = icon("home")
      ),
    menuItem(
      "Temperature Radials",
      tabName = "dashboard5",
      icon = icon("thermometer-quarter")
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
      "Ridgeline Plot",
      tabName = "dashboard3",
      icon = icon("dashboard")
    ),
    menuItem(
      "Trends",
      icon = icon("chart-line"),
      menuItem(
        "Temperature Trends",
         tabName = "dashboard6",
         icon = icon("chart-line")), 
      menuItem(
        "Tempertaure Trend Animated",
        tabName = "dashboard8",
        icon = icon("chart-line")
      )
    ),
    menuItem(
      "Calendar Heatmap",
      tabName = "dashboard7",
      icon = icon("calendar")
    )
,
    menuItem(
      "Climate Choropleth Map",
      tabName = "dashboard1",
      icon = icon("map")
    )
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~  2.1.1 define dashboard body elements ~~~~~~~~~~~~~~~~~~~~~~~~~ 
homepage <- tabItem(tabName = "homepage",
                    fluidPage(
                      fluidRow(
                        HTML('<center><img src="Rain_Shine.png" width="300"></center>')
                        
                      ),
                      fluidRow(column(12, h2(
                        "Problem and Motivation"
                      )),
                      column(
                        12,
                        h5(
                          " The current reporting of Singapore's climate has always been  primitive, hence, it is challenging for users to derive in-depth  insights. In 2019, multiple news companies reported that Singapore is heating up twice as fast as the rest of the world and that Professor Matthias Roth from the department of geography at National  University of Singapore attributed the rising temperatures to global warming and the Urban Heat Island (UHI) effect. However, there was no data or charts provided from them to back up their claims on  Singapore's climate change."
                        )
                      ), ),
                      fluidRow(column(
                        12,
                        h5(
                          "Our team aims to present Singapore's climate data in more user-friendly and meaningful interpretation ways. Through Rain&Shine, an interactive and user-friendly visualization dashboard, that shows the distribution of the climate by Subzone, Region, and Singapore as a whole, we hope to provide Singaporeans with knowledge and in-depth insights into Singapore's Climate. Additionally, we want to identify the trends inherent within the weather data available and answer questions regarding the changes in Singapore's climate from available historical data."
                        )
                      )),
                      fluidRow(column(12, h6("Data Source: Weather.gov")))
                    ))

dashboard1 <- tabItem(tabName = "dashboard1",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel(
                          "Birdeye view of Mean Rainfall and Mean Temperature Using Singapore Map"
                        ),
                        fluidRow(sidebarLayout(
                          sidebarPanel(fluidRow(uiOutput("sYear")),
                                       fluidRow(uiOutput("sMonth")),
                                       width = 2),
                          mainPanel(fluidRow(
                            column(5, leafletOutput("lxmap")),
                            column(5, leafletOutput("lxmap2"))
                          ), width = 10)
                        )),
                        fluidRow(column(
                          12,
                          h4(
                            "DISCLAIMER: Our goal is to create an isopleth map. However, due to the limited datapoint available and limited knowledge within the team, we are unable to achieve our goal. Here's our attempt with a choropleth map."
                          )
                        ))
                      ))

dashboard2 <- tabItem(tabName = "dashboard2",
                      withSpinner(fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Distribution of the Measurements and Probability Density"),
                        fluidRow(
                          column(5, uiOutput('tMeasure')),
                          column(5, uiOutput("tYear1"))),
                        fluidRow(withSpinner(plotlyOutput(
                          "tanny1", width = "80%", height = "400px"
                        )))
                      )))

dashboard3 <- tabItem(tabName = "dashboard3",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Joyplot to Visualize the Changes in Distribution Over the Years"),
                        fluidRow(uiOutput("tYear3"),
                                 fluidRow(
                                   column(6, plotOutput(
                                     "tanny2", width = "100%", height = "500px"
                                   )),
                                   column(6, plotOutput(
                                     "tanny3", width = "100%", height = "500px"
                                   ))
                                 ))
                      ))
dashboard4 <- tabItem(tabName = "dashboard4",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Understanding the Relationship of Singapore's Climate"),
                        fluidRow(),
                        fluidRow(
                          column(4,uiOutput("tYear")),
                          column(4,uiOutput("tRegion1"))),
                        fluidRow(withSpinner(plotlyOutput("tanny4"))),
                        fluidRow(img(src='scatter.png', align = "left", width="400px", height = "30px"),)
                      ))

dashboard5 <- tabItem(tabName = "dashboard5",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Temperature Radials"),
                        fluidRow(
                          column(4, uiOutput('hcr')),
                          column(4, uiOutput('hcRegion'))),
                        fluidRow(column(
                          6,
                          withSpinner( highchartOutput("hc", width = "100%", height = "400px"))
                        )),
                        fluidRow(img(src='Viridis.png', align = "right", width="300px", height = "65px"),)
                      ))

dashboard6 <- tabItem(tabName = "dashboard6",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Singapore Temperature Change (1990-2019)"),
                        fluidRow(withSpinner(
                          highchartOutput("hc2", height = "550px")
                        )),
                        fluidRow(img(src='Viridis.png', align = "right", width="300px", height = "65px"),)
                      ))

dashboard7 <- tabItem(tabName = "dashboard7",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Heatmap Across the Years"),
                        fluidRow(column(4, uiOutput("my_measure")),
                                 column(4, uiOutput("my_calendar"))),
                        fluidRow(withSpinner(plotOutput("my1")))
                      ))

dashboard8 <- tabItem(tabName = "dashboard8",
                      fluidPage(tags$style(type = "text/css", css),
                                fluidRow(withSpinner(plotOutput("my2")))))
                                
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
# mainDF <- mainDF %>%
#   filter(!str_detect(mainDF$Measurement, "Wind")) %>%
#   filter(Year >= 1990) 
# mainDF <- readRDS("shiny data/mainDF.RDS")
#-------------- Non Maps ------------
#------Rridge-------
# tm <- mainDF %>%
#   mutate(
#     Month = fct_relevel(
#       Month,
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
#     )
#   )
# masterDF <- tm %>%
#   filter(str_detect(tm$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Year, Month, Region, SZ) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# tm_tmp <- tm %>%
#   filter(str_detect(tm$Measurement, "Mean Temperature")) %>%
#   group_by(Year, Month, Region, SZ) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# masterDF$mean_temp = tm_tmp$mean_temp
# masterDF <- masterDF %>%
#   mutate_at(vars(mean_temp,
#                  mean_rain, ),
#             funs(round(., 1)))

masterDF <- readRDS("shiny data/masterDF.RDS")

#----- Correlation ------
# masterDF3 <- tm %>%
#   filter(str_detect(tm$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Year,Month,Region) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# 
# tm_tmp2 <- tm %>%
#   filter(str_detect(tm$Measurement, "Mean Temperature")) %>%
#   group_by(Year,Month,Region) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# 
# masterDF3$mean_temp = tm_tmp2$mean_temp
# 
# masterDF3 <- masterDF3 %>%
#   mutate_at(vars(mean_temp,
#                  mean_rain,),
#             funs(round(., 1))) %>%
#   na.omit()

masterDF3 <- readRDS("shiny data/masterDF3.RDS")

#----- Violin ------
# tmpV <- mainDF %>%
#   select(Year, Month, Region, SZ, Measurement, Value) %>%
#   mutate(Month = fct_relevel(Month,
#                              "Jan","Feb","Mar",
#                              "Apr","May","Jun",
#                              "Jul","Aug","Sep",
#                              "Oct","Nov","Dec"))
tmpV <- readRDS("shiny data/tmpV.RDS")

#-------------- Maps----------------
# masterDF2  <- tm %>%
#   filter(str_detect(tm$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Year, Month, SZ) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# df2Temp <- tm %>%
#   filter(str_detect(tm$Measurement, "Mean Temperature")) %>%
#   group_by(Year, Month, SZ) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# masterDF2$mean_temp = df2Temp$mean_temp
# masterDF2 <- masterDF2 %>%
#   mutate_at(vars(mean_temp, mean_rain), funs(round(., 1)))

masterDF2 <- readRDS("shiny data/masterDF2.RDS")

#--------------Weathers trends------------
# Mastertemp <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Temperature")) %>%
#   group_by(Year, Month) %>%
#   summarise(
#     median = median(Value, na.rm = TRUE),
#     lower = min(Value, na.rm = TRUE),
#     upper = max(Value, na.rm = TRUE),
#     avg = mean(Value, na.rm = TRUE)
#   ) %>%
#   na.omit()
# Mastertemp$date <-
#   as.Date(with(Mastertemp, paste(Year, Month, "01", sep = "-")), "%Y-%b-%d")
# smooth_vals <- predict(loess(median ~ Year, Mastertemp))
# Mastertemp$smooth_vals <- smooth_vals
# Mastertemp <- Mastertemp %>%
#   mutate_at(c(3:6, 8), funs(round(., 1))) 

# x <- c("Max: ", "Median: ", "Min: ", "Predict: ")
# y <-
#   sprintf("{point.%s}", c("upper", "median", "lower", "smooth_vals"))
# tltip <- tooltip_table(x, y)
# 
# median_tmp <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Temperature"))
# 
# med <- median(median_tmp$Value, na.rm = TRUE)
# low <- min(median_tmp$Value, na.rm = TRUE)
# high = max(median_tmp$Value, na.rm = TRUE)
# 
# trendChart <- hchart(Mastertemp,
#                      type = "columnrange",
#                      hcaes(
#                        x = date,
#                        low = lower,
#                        high = upper,
#                        color = smooth_vals
#                      )) %>%
#   hc_yAxis(
#     tickPositions = c(low - 5, med, high + 5),
#     gridLineColor = "#000000",
#     labels = list(format = "{value} C", useHTML = TRUE)
#   ) %>%
#   hc_add_series(
#     data = Mastertemp,
#     type = "line",
#     hcaes(x = date, y = smooth_vals),
#     color = "#B71C1C"
#   ) %>%
#   hc_tooltip(
#     useHTML = TRUE,
#     headerFormat = as.character(tags$small("{point.x: %Y %b}")),
#     pointFormat = tltip
#   )

#--------------Weathers Radials------------
# Mastertemp2 <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Temperature")) %>%
#   select(Region, Year, date, Value) %>%
#   group_by(Year, date, Region) %>%
#   summarise(
#     min_temperaturec = min(Value, na.rm = TRUE),
#     max_temperaturec = max(Value, na.rm = TRUE),
#     mean_temperaturec = mean(Value, na.rm = TRUE),
#     median_temperaturec = median(Value, na.rm = TRUE)
#   ) %>%
#   na.omit()
# smooth_vals <- predict(loess(median_temperaturec ~ Year, Mastertemp2))
# Mastertemp2$smooth_vals <- smooth_vals
# Mastertemp2 <- Mastertemp2 %>%
#   mutate_at(4:8, funs(round(., 1)))

Mastertemp2 <- readRDS("shiny data/Mastertemp2.RDS")
Year_min <- min(Mastertemp2[, "Year"], na.rm = TRUE)
Year_max <- max(Mastertemp2[, "Year"], na.rm = TRUE)
#-------------- Calendar Heatmap ----------------
# dat <- mainDF %>%
#   select(Year, Month, Day, Measurement, Value) %>%
#   filter(str_detect(mainDF$Measurement, "Mean Temperature|Daily Rainfall Total")) %>%
#   filter(!is.na(Value))
# dat$date <- as.Date(with(dat, paste(Year, Month, Day, sep = "-")), "%Y-%b-%d")
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
      animate = animationOptions(interval = 5000, loop = FALSE)
    )
  })
  
  output$my_measure <- renderUI({
    selectInput(
      inputId = "my_measure",
      label = "Select to view:",
      choices = c('Daily Rain Precipitation Total (mm)', "Mean Temperature (°C)")
    )
  })
  output$my1 <- renderPlot({
    if (input$my_measure == "Daily Rain Precipitation Total (mm)") {
      dat %>% 
        filter(str_detect(Measurement,"Rain")) %>% 
        filter(Year <= input$calendaryear[2]) %>% 
        filter(Year >= input$calendaryear[1]) %>% 
        ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + 
        facet_grid(Year ~ monthf) + 
        scale_fill_gradient(low = "green", high = "red") +
        labs(fill = input$my_measure) +
        xlab("Week of Month") + ylab("")
    }
    else {
      dat %>% 
        filter(str_detect(Measurement,"Mean Temperature")) %>% 
        filter(Year <= input$calendaryear[2]) %>% 
        filter(Year >= input$calendaryear[1]) %>% 
        ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + 
        facet_grid(Year ~ monthf) + 
        scale_fill_gradient(low = "green", high = "red") +
        labs(fill = input$my_measure) + 
        xlab("Week of Month") + ylab("")
    }
  })
  #----------------------------------------dashboard 6 Climate Trend ---------------------------------------
  output$hc2 <- renderHighchart({
    trendChart<- readRDS("shiny data/trendChart.RDS")
  })
  
  #----------------------------------------dashboard 5 Temperature Radials ---------------------------------------
  output$hcr <- renderUI({
    sliderInput(
      inputId = "hc_Year",
      label = "Year:",
      min =  Year_min,
      max = Year_max,
      value = Year_min,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000, loop = FALSE)
    )
  })
  
  output$hcRegion <- renderUI({
    tmp <- Mastertemp2 %>%
      filter(Year == as.numeric(input$hc_Year)) %>%
      distinct(Region)
    
    selectInput(
      inputId = "hc_Region",
      label = "Select Region:",
      choices = c("All",tmp$Region),
      selected = "All"
    )
  })
  
  output$hc <- renderHighchart({
    x <- c("Min", "Mean", "Max")
    y <- sprintf("{point.%s}", c("min_temperaturec", "mean_temperaturec", "max_temperaturec"))
    tltip <- tooltip_table(x, y)
    
    if (input$hc_Region != "All") {
      Mastertemp3 <- Mastertemp2 %>%
        filter(Year == as.character(input$hc_Year)) %>%
        filter(Region == as.character(input$hc_Region))
    }else{
      # Mastertemp3 <- Mastertemp2 %>%
      #   group_by(Year, date) %>%
      #   summarise(
      #     min_temperaturec = min(min_temperaturec, na.rm = TRUE),
      #     max_temperaturec = max(max_temperaturec, na.rm = TRUE),
      #     mean_temperaturec = mean(mean_temperaturec , na.rm = TRUE),
      #     median_temperaturec = median(median_temperaturec, na.rm = TRUE)
      #   ) %>%
      #   na.omit()
      # 
      # smooth_vals <-
      #   predict(loess(median_temperaturec ~ Year, Mastertemp3))
      # Mastertemp3$smooth_vals <- smooth_vals
      # 
      # Mastertemp3 <- Mastertemp3 %>%
      #   mutate_at(3:7, funs(round(., 1)))
      Mastertemp3<- readRDS("shiny data/Mastertemp3.RDS") %>%
        filter(Year == as.character(input$hc_Year))
    }
    
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
      animate = animationOptions(interval = 5000,loop = FALSE)
    )
  })
  
  output$tRegion1 <- renderUI({
    db4_tmp <- masterDF %>%
      filter(Year == input$YearTanny4)%>%
      na.omit() %>%
      distinct(Region)
    
    selectInput(
      inputId = "db4select",
      label = "Please Select Region:",
      choices = c("All", db4_tmp$Region),
      selected = "All"
    )
  })

  tanny4 <- reactive({
    if(input$db4select != "All"){
      tanny4_tmp<- masterDF3 %>%
        filter(Year == input$YearTanny4)%>%
        filter(Region == input$db4select)
    }else
    {
      tanny4_tmp<- masterDF3 %>%
        filter(Year == input$YearTanny4)
    }
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
      scale_color_manual(
        values = c(
          'East' = "#E7B800",
          'Central' = "#6f7778",
          'North' = "#FC4E07",
          'North-East' = "#293352",
          'West' = "#52854C"
        )
      ) +
      theme(legend.position = "none") +
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
      min = Year_min,
      max = Year_max,
      value = c(Year_min, Year_min + 10),
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  output$tanny2 <- renderPlot({
    masterDF %>%
      filter(Year <= input$YearTanny3[2]) %>% 
      filter(Year >= input$YearTanny3[1]) %>% 
      ggplot(aes(x = mean_temp, y = factor(Year), fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Temperature (\u00B0C)", option = "B") +
      labs(title = 'Temperature',
           subtitle = 'Mean temperatures (Celcius) by Year',
           x ="Mean Temperature") +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(axis.title.y = element_blank()) + 
      xlim(min(masterDF$mean_temp, na.rm = TRUE) - 1,
          max(masterDF$mean_temp, na.rm = TRUE) + 1)
  })
  
  output$tanny3 <- renderPlot({
    masterDF %>%
      filter(Year <= input$YearTanny3[2]) %>% 
      filter(Year >= input$YearTanny3[1]) %>% 
      ggplot(aes(x = mean_rain, y = factor(Year), fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Precipitation (mm)", option = "D") +
      labs(title = 'Rainfall',
           subtitle = 'Mean Rain Precipitation (mm) by Year',
           x ="Mean Rainll") +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(axis.title.y = element_blank())
  })
  
  #----------------------------------------dashboard 2 Voilin Plot  ---------------------------------------
  output$tMeasure <- renderUI({
    mchoices <- tmpV %>%
      select(Measurement) %>%
      na.omit() %>%
      distinct(Measurement)
    
    selectInput(
      inputId = "db2type",
      label = "Select to view:",
      choices = c(mchoices),
      selected = mchoices[1]
    )
  })

  output$tYear1 <- renderUI({
    sliderInput(
      inputId = "YearTanny1",
      label = "Year:",
      min = if (str_detect(input$db2type, "Temperature")) {
        2009
      } else{
        min(
          tmpV %>%
            select(Measurement, Year,Value) %>%
            filter(str_detect(tmpV$Measurement, input$db2type)) %>%
            na.omit() %>%
            select(Year),
          na.rm = TRUE
        )
      },
      max = 2019,
      value = if (str_detect(input$db2type, "Temperature")) {
        2009
      } else{
        min(
          tmpV %>%
            select(Measurement, Year,Value) %>%
            filter(str_detect(tmpV$Measurement, input$db2type)) %>%
            na.omit() %>%
            select(Year),
          na.rm = TRUE
        )
      },
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000, loop = FALSE)
    )
  })

  output$tanny1 <- renderPlotly({
    rain <- ggplot(tmpV %>%
                     filter(Year == as.numeric(input$YearTanny1)) %>%
                     filter(str_detect(Measurement, input$db2type)) %>%
                     group_by(Year, Month, Region, SZ) %>%
                     summarise(mean_valuedb2 = mean(Value, na.rm = TRUE)) %>%
                     na.omit(),
                   aes(x = Month,
                       y = mean_valuedb2
                       )
                   ) +
      geom_violin(
        color = "purple",
        add = "boxplot",
        fill = "purple",
        alpha = 0.5,
        ) +
      geom_boxplot(
        width = 0.2,
        fill = "grey",
        alpha = 1
        ) +
      xlab("") +
      ylab(if (str_detect(input$db2type,"Rainfall")) {
        "Rain Precipitation ( mm )"
      } else{
        "Temperature (\u00B0C)"
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
      animate = animationOptions(interval = 5000,loop = FALSE)
    )
  })
  
  output$sMonth <- renderUI({
    selectInput(
      inputId = "MonthLX",
      label = "Month:",
      choices = c("All", "Jan","Feb","Mar",
                  "Apr","May","Jun",
                  "Jul","Aug","Sep",
                  "Oct","Nov","Dec"),
      selected = "All"
    )
  })
  
  tmp<-reactive({
    tmp2 <- masterDF2 %>%
      filter(Year == as.numeric(input$YearLX))
    
    if(input$MonthLX != "All"){
      tmp2 <- tmp2 %>%
        filter(Month == as.character(input$MonthLX))
    }
    
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