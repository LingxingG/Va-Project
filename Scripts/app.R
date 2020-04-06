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
                      fluidPage(titlePanel("Tmap"),
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput("sYear"),
                                    uiOutput("sMonth")
                                  ),
                                  mainPanel(leafletOutput("mymap"))
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
  rainfall_mean <- mainDF %>%
    filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
    group_by(Region, SZ, Station, Year, Month) %>%
    summarise(mean_rain = mean(Value, na.rm = TRUE),
              total_rain = sum(Value, na.rm = TRUE))
  
  output$sYear <- renderUI({
    Year_choices <- as.character(unique(rainfall_mean$Year))
    Year_choices <- Year_choices[-length(Year_choices)]
    Year_choices <- str_sort(Year_choices,decreasing = TRUE, numeric = TRUE)
    selectInput(inputId = "YearLX", label = "Year:", choices = Year_choices, selected = "2019")
  })
  
  output$sMonth <- renderUI({
    Month_choices <- as.character(unique(rainfall_mean$Month))
    Month_choices <- Month_choices[-length(Month_choices)]
    selectInput(inputId = "MonthLX", label = "Month:", choices = Month_choices, selected = "Jan")
  })
  
  tmp <- rainfall_mean %>%
    filter(Year == 2019) %>%
    filter(Month == 'Jan')
  
  tmp <- left_join(mpsz, tmp,
                   by = c("SUBZONE_N" = "SZ"))
  
  tmp <- st_transform(tmp, 4326)
  output$mymap = renderLeaflet({
    tm <- tm_shape(tmp) +
      tm_fill("mean_rain",
              n = 6,
              style = "quantile",
              palette = "Blues") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  observeEvent( c(input$YearLX,input$MonthLX), {
    
    ## get the choice from the drop-down box
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
    }else{
      tmp_new <- tmp
    }
    
    ## plot the subsetted ata
    output$mymap = renderLeaflet({
      tm <- tm_shape(tmp_new)+
        tm_fill("mean_rain",
                n = 6,
                style = "quantile", 
                palette = "Blues") +
        tm_borders(alpha = 0.5)
      tmap_leaflet(tm)
    })
  })
}
######################### 4. Finish app ##########################
shinyApp(ui,server)
