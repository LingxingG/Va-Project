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
  'tmaptools',
  'shinydashboard',
  'ggstatsplot',
  'ggplot2',
  'gganimate',
  'ggridges',
  "gridExtra",
  "htmlwidgets",
  "shinythemes",
  "forcats",
  'gganimate',
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
    )
    # menuItem(
    #   "Dashboard 5",
    #   tabName = "dashboard5",
    #   icon = icon("dashboard")
    # ),
    # menuItem(
    #   "Dashboard 6",
    #   tabName = "dashboard6",
    #   icon = icon("dashboard")
    # )
  )
)

### 2.1.1 dfine dashboard body elements ###

homepage <- tabItem(tabName = "homepage",
                      fillPage(theme = shinytheme("united"),
                               title = "Tmap",
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


# dashboard5 <- tabItem(tabName = "dashboard5",
#                       fluidPage(theme = shinytheme("united"),
#                                 title = "mmap",
#                                 fluidRow(plotOutput("my1")),
#                                 fluidRow(
#                                   column(4, uiOutput("my_measure")),
#                                   column(4, uiOutput("my_calendar"))
#                                   )
#                                 ))
# 
# dashboard6 <- tabItem(tabName = "dashboard6",
#                       fluidPage(theme = shinytheme("united"),
#                                 title = "mmap",
#                                 fluidRow(plotOutput("my2")
#                                 )))

### 2.1.2 Fill in dashboard elements ####
body <- dashboardBody(tabItems(# First tab content
  homepage
  # dashboard5,
  # dashboard6
  ))

ui <- dashboardPage(header, sidebar, body, skin="black")

######################### 3. define input output ##########################
# mpsz <- st_read(dsn = "geospatial",
#                 layer = "MP14_SUBZONE_WEB_PL")
# mainDF <- read.csv("merged data\\dataset.csv")
# 
# # declare base dataframe to use for both rain and temp
# rainfall <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
#   group_by(Region, SZ, Station,Year, Month) %>%
#   summarise(mean_rain = mean(Value, na.rm = TRUE))
# 
# temperature <- mainDF %>%
#   filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
#   group_by(Region, SZ, Station, Year, Month) %>%
#   summarise(mean_temp = mean(Value, na.rm = TRUE))
# 
# masterDF <- rainfall %>%
#   mutate(Month = fct_relevel(Month, 
#                              "Jan","Feb","Mar",
#                              "Apr","May","Jun",
#                              "Jul","Aug","Sep",
#                              "Oct","Nov","Dec"))
# masterDF$mean_temp = temperature$mean_temp
# 
# calendar <- mainDF
# calendar$date <- as.Date(with(calendar, paste(Year, Month, Day,sep="-")), "%Y-%b-%d")
# dat <- calendar %>% filter(!is.na(Value))
# dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
# dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
# dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# dat$weekday = as.POSIXlt(dat$date)$wday
# dat$weekdayf<-factor(dat$weekday,levels=rev(0:6),labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
# dat$yearmonth<-as.yearmon(dat$date)
# dat$yearmonthf<-factor(dat$yearmonth)
# dat$week <- as.numeric(format(dat$date,"%W"))
# dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# temperature_my<- mainDF %>% filter(Measurement == "Mean Temperature (Â°C)") %>% filter(!is.na(Value))
# temperature_my$date <- as.Date(with(temperature_my, paste(Year, Month, Day,sep="-")), "%Y-%b-%d")
# smooth_vals = predict(loess(Value~Year,dat))
# dat$smooth_vals <- smooth_vals
######################### 3.1 define customer function ##########################

### 3.1 import attribute data ###
server <- function(input, output) {
  # #----------------------------------------dashboard 6---------------------------------------
  # output$my2 <- renderImage({
  #   list(src = "temperature_trend.gif",
  #        contentType = 'image/gif'
  #        # width = 400,
  #        # height = 300,
  #        # alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  # 
  # #----------------------------------------dashboard 5---------------------------------------
  # output$my_calendar <- renderUI({
  #   Year_min <- min(dat[, "Year"], na.rm = TRUE)
  #   Year_max <- max(dat[, "Year"], na.rm = TRUE)
  #   sliderInput(
  #     inputId = "calendaryear",
  #     label = "Select time period: ",
  #     min = Year_min,
  #     max = Year_max,
  #     value = c(Year_min,Year_min+3),
  #     step = 1,
  #     sep = "",
  #     animate = animationOptions(interval = 5000,
  #                                loop = FALSE)
  #   )
  # })
  # 
  # output$my_measure <- renderUI({
  #   selectInput(
  #     inputId = "my_measure",
  #     label = "Select to view",
  #     choices = c('Daily Rainfall Total (mm)',"Mean Temperature (°C)")
  #   )
  # })
  # output$my1 <- renderPlot({
  #   if(input$my_measure == "Daily Rainfall Total (mm)"){
  #     dat %>% filter(Measurement == input$my_measure) %>% filter(Year <= input$calendaryear[2])%>% filter(Year >= input$calendaryear[1]) %>% ggplot(aes(monthweek, weekdayf, fill = Value)) + 
  #       geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="yellow", high="red") +
  #       labs(title = "Heatmap Across the Years",fill=input$my_measure) +  xlab("Week of Month") + ylab("")
  #   }
  #   else {
  #     dat %>% filter(Measurement == "Mean Temperature (Â°C)") %>% filter(Year <= input$calendaryear[2])%>% filter(Year >= input$calendaryear[1]) %>% ggplot(aes(monthweek, weekdayf, fill = Value)) + 
  #       geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="yellow", high="red") +
  #       labs(title = "Heatmap Across the Years",fill=input$my_measure) +  xlab("Week of Month") + ylab("")
  #     
  #   }
  #   
  # })
  

}
######################### 4. Finish app ##########################
shinyApp(ui,server)