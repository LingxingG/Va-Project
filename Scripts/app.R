####################### 1. define Package to install #######################
packages = c(
  'dplyr',
  'tidyverse',
  'sf',
  'tmap',
  'leaflet',
  'plotly',
  'shiny',
  'stringr',
  'tmaptools',
  'shinydashboard'
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

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard1",
    tabName = "dashboard1",
    icon = icon("dashboard")
  ),
  menuItem(
    "Dashboard2",
    tabName = "dashboard2",
    icon = icon("dashboard")
  )
))

### 2.1.1 dfine dashboard body elements ###
dashboard1 <- tabItem(tabName = "dashboard1",
                      fillPage(title = "Tmap",
                                fluidRow(
                                  column(4, leafletOutput("lxmap")),
                                  column(4, leafletOutput("lxmap2"))
                                ),
                                br(),
                                fluidRow(
                                  column(4, uiOutput("sYear")),
                                  column(4, uiOutput("sMonth"))
                                )))

### 2.1.2 Fill in dashboard elements ####
body <- dashboardBody(dashboardBody(tabItems(
  
  # First tab content
  dashboard1
  # 
  # # Second tab content
  # dashboard2
  )))

ui <- dashboardPage(header, sidebar, body, skin="black")

######################### 3. define input output ##########################
mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")
mainDF <- read.csv("merged data\\dataset.csv")

### 3.1 import attribute data ###
server <- function(input, output) {
  
  #----------------------------------------dashboard 1----------------------------------------
  
  # declare base dataframe to use for both rain and temp
  rainfall_mean <- mainDF %>%
    filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
    group_by(Region, SZ, Station,Year, Month) %>%
    summarise(mean_rain = mean(Value, na.rm = TRUE))
  
  temperature_mean <- mainDF %>%
    filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
    group_by(Region, SZ, Station, Year, Month) %>%
    summarise(mean_temp = mean(Value, na.rm = TRUE))
  # 
  # output$sMeasure <- renderUI({
  #   selectInput(
  #     inputId = "MeasurementLX",
  #     label = "Choose a Measurement:",
  #     choices = list(
  #       "Rain" = list(
  #         "Daily Rainfall Total",
  #         "Highest 30 Min Rainfall",
  #         "Highest 60 Min Rainfall",
  #         "Highest 120 Min Rainfall"
  #       ),
  #       "Temperature" = list(
  #         "Mean Temperature",
  #         "Maximum Temperature",
  #         "Minimum Temperature"
  #       ),
  #       "Wind" = list("Mean Wind Speed", "Max Wind Speed")
  #     ),
  #     selected = "Daily Rainfall Total"
  #   )
  # })
  
  # render filters in UI 
  output$sYear <- renderUI({
    Year_min <- min(rainfall_mean[, "Year"], na.rm = TRUE)
    Year_max <- max(rainfall_mean[, "Year"], na.rm = TRUE)
    sliderInput(
      inputId = "YearLX",
      label = "Year:",
      min = Year_min,
      max = Year_max,
      value = c(Year_min),
      step = 1,
      sep = "",
      animate = animationOptions(interval = 5000,
                                 loop = FALSE)
    )
  })
  
  output$sMonth <- renderUI({
    Month_choices <- c("Jan","Feb","Mar",
                            "Apr","May","Jun",
                            "Jul","Aug","Sep",
                            "Oct","Nov","Dec")
    selectInput(
      inputId = "MonthLX",
      label = "Month:",
      choices = Month_choices,
      selected = "Jan"
    )
  })
  
  
  ## define default variable for rain map
  tmp <- rainfall_mean %>%
    filter(Year == 2019) %>%
    filter(Month == 'Jan')
  tmp <- left_join(mpsz, tmp,
                   by = c("SUBZONE_N" = "SZ"))
  
  tmp <- st_transform(tmp, 4326)
  
  # show rain map
  output$lxmap = renderLeaflet({
    tm <- tm_shape(tmp)+
      tm_fill("mean_rain",
              style = "quantile",
              palette = "Blues") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  # define default variable for temp map
  tmp_temp <- temperature_mean %>%
    filter(Year == 2019) %>%
    filter(Month == 'Jan')

  tmp_temp <- left_join(mpsz, tmp_temp,
                   by = c("SUBZONE_N" = "SZ"))
  
  tmp_temp <- st_transform(tmp_temp, 4326)
  
  # show temp map
  output$lxmap2 = renderLeaflet({
    tm <- tm_shape(tmp_temp)+
      tm_fill("mean_temp",
              style = "quantile",
              palette = "Oranges") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  #reactive event
  observeEvent(c(input$YearLX,input$MonthLX), {
    
    YEAR = as.numeric(input$YearLX)
    MONTH = as.character(input$MonthLX)

    ## supbset the data based on the choice
    if(YEAR != 2019 || MONTH != 'Jan'){
       tmp_new<- rainfall_mean %>%
         filter(Year == YEAR) %>%
         filter(Month == MONTH)
        
       tmp_new <- left_join(mpsz, tmp_new,
                            by = c("SUBZONE_N" = "SZ"))
       tmp_new <- st_transform(tmp_new, 4326)
       
       tmp_temp_new<- temperature_mean %>%
         filter(Year == YEAR) %>%
         filter(Month == MONTH)
       
       tmp_temp_new <- left_join(mpsz, tmp_temp_new,
                            by = c("SUBZONE_N" = "SZ"))
       tmp_temp_new <- st_transform(tmp_temp_new, 4326)
       
       
    }else{
      tmp_new <- tmp
      tmp_temp_new <- tmp_temp
    }
    
    # plot the subsetted ata
    output$lxmap = renderLeaflet({
      tm <- tm_shape(tmp_new)+
        tm_fill("mean_rain",
                style = "quantile",
                palette = "Blues") +
        tm_borders(alpha = 0.5)
      tmap_leaflet(tm)
    })
    
    output$lxmap2 = renderLeaflet({
      tm <- tm_shape(tmp_temp_new)+
        tm_fill("mean_temp",
                style = "quantile",
                palette = "Oranges") +
        tm_borders(alpha = 0.5)
      tmap_leaflet(tm)
    })
  })
}
######################### 4. Finish app ##########################
shinyApp(ui,server)
