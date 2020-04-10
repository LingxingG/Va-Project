####################### 1. define Package to install #######################
packages = c(
  'plyr',
  'dplyr',
  'tidyverse',
  'sf',
  'tmap',
  'leaflet',
  'plotly',
  'shiny',
  'shinyWidgets',
  'stringr',
  'shinydashboard',
  'ggplot2',
  'ggridges',
  "htmlwidgets",
  "forcats",
  'transformr',
  'gifski',
  'png',
  'quantmod',
  'reshape2',
  'scales'
)

for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

######################### 2. define dashboard UI ##########################

### 2.1 define dashboard elemets ###
header <- dashboardHeader(title = "Rain and Shiny Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Homepage",
      tabName = "homepage",
      icon = icon("dashboard")
    ),
    menuItem(
      "Dashboard 5",
      tabName = "dashboard7",
      icon = icon("dashboard")
    ),
    menuItem(
      "Dashboard 6",
      tabName = "dashboard8",
      icon = icon("dashboard")
    )
  )
)

### 2.1.1 dfine dashboard body elements ###

homepage <- tabItem(tabName = "homepage",
                      fillPage(
                               fluidRow(
                                 HTML('<center><img src="Rain_Shine.png" width="400"></center>')
                                 
                               ),
                               fluidRow(
                                 tags$div(
                                   tags$h2(tags$b("Motivation"),align="center"),
                                   tags$p("Although the weather in Singapore does not seem to be an essential part of everyone's lifestyle, the weather can play an important role in affecting one's health."),
                                   tags$p("Our team aims to present Singapore's weather data in more user-friendly and meaningful interpretation ways. Through our user-friendly dashboards visualization, we hope to provide users in-depth insights, to identify the trends and patterns inherent within the weather data available, and answer questions regarding the changes in Singapore's weather from available historical data.") ,
                                   tags$p()
                                   
                                 )
                               )))


dashboard7 <- tabItem(tabName = "dashboard7",
                      fluidPage(fluidRow(
                        column(4, uiOutput("my_measure")),
                        column(4, uiOutput("my_calendar"))
                      ),
                      fluidRow(plotOutput("my1"))))

dashboard8 <- tabItem(tabName = "dashboard8",
                      fluidPage(fluidRow(plotOutput("my2"))))

### 2.1.2 Fill in dashboard elements ####
body <- dashboardBody(tabItems(
  homepage,
  dashboard7,
  dashboard8
  ))

ui <- dashboardPage(header, sidebar, body, skin="black")

######################### 3. define input output ##########################
mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")
mainDF <- read_csv("merged data/dataset.csv")


dat <- mainDF %>%
  select(Year, Month, Day, Measurement, Value) %>%
  filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
  filter(!is.na(Value))

dat$date <- as.Date(with(dat, paste(Year, Month, Day, sep = "-")), "%Y-%b-%d")

dat$Year <- as.numeric(as.POSIXlt(dat$date)$year + 1900)
dat$Month <- as.numeric(as.POSIXlt(dat$date)$mon + 1)
dat$monthf <-
  factor(
    dat$Month,
    levels = as.character(1:12),
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    ),
    ordered = TRUE
  )
dat$weekday = as.POSIXlt(dat$date)$wday
dat$weekdayf <-
  factor(
    dat$weekday,
    levels = rev(0:6),
    labels = rev(c(
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
    )),
    ordered = TRUE
  )
dat$yearmonth <- as.yearmon(dat$date)
dat$yearmonthf <- factor(dat$yearmonth)
dat$week <- as.numeric(format(dat$date, "%W"))
dat <- ddply(dat, .(yearmonthf), transform, monthweek = 1 + week - min(week))

smooth_vals = predict(loess(Value ~ Year, dat))
dat$smooth_vals <- smooth_vals
######################### 3.1 define customer function ##########################

### 3.1 import attribute data ###
server <- function(input, output) {
  # #----------------------------------------dashboard 8---------------------------------------
  output$my2 <- renderImage({
    list(src = "temperature_trend.gif",
         contentType = 'image/gif'
    )})

  #----------------------------------------dashboard 7---------------------------------------
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
      dat %>% filter(Measurement == input$my_measure) %>% filter(Year <= input$calendaryear[2]) %>% filter(Year >= input$calendaryear[1]) %>% ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + facet_grid(year ~ monthf) + scale_fill_gradient(low =
                                                                                        "yellow", high = "red") +
        labs(title = "Heatmap Across the Years", fill = input$my_measure) +  xlab("Week of Month") + ylab("")
    }
    else {
      dat %>% filter(Measurement == "Mean Temperature (Â°C)") %>% filter(Year <= input$calendaryear[2]) %>% filter(Year >= input$calendaryear[1]) %>% ggplot(aes(monthweek, weekdayf, fill = Value)) +
        geom_tile(colour = "white") + facet_grid(year ~ monthf) + scale_fill_gradient(low =
                                                                                        "yellow", high = "red") +
        labs(title = "Heatmap Across the Years", fill = input$my_measure) +  xlab("Week of Month") + ylab("")
    }
    
  })
}
######################### 4. Finish app ##########################
shinyApp(ui,server)