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
                                    uiOutput("sYear")
                                  ),
                                  mainPanel(leafletOutput("mymap", height =550))
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

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

### 3.1 import attribute data ###
server <- function(input, output) {
  
  #----------------------------------------dashboard 1----------------------------------------
  rainfall_mean <- mainDF %>%
    filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
    group_by(Region, SZ, Station, Year, Month) %>%
    summarise(mean_rain = mean(Value, na.rm = TRUE),
              total_rain = sum(Value, na.rm = TRUE))
  
  output$sYear <- renderUI({
    choices <- as.character(unique(rainfall_mean$Year))
    choices <- choices[-length(choices)]
    choices <- str_sort(choices,decreasing = TRUE, numeric = TRUE)
    selectInput(inputId = "YearLX", label = "Year:", choices = choices, selected = "2019")
  }) 
  
  tmp <- rainfall_mean %>%
    filter(Year == 2019)
  
  tmp <- left_join(mpsz, tmp,
                   by = c("SUBZONE_N" = "SZ"))
  
  output$mymap = renderLeaflet({
    tm <- tm_shape(tmp) +
      tm_fill(
        "mean_rain",
        style = "quantile",
        palette = "Blues",
        legend.hist = TRUE,
        legend.is.portrait = TRUE,
        legend.hist.z = 0.1
      ) +
      tm_layout(
        legend.height = 0.45,
        legend.width = 0.35,
        legend.outside = FALSE,
        legend.position = c("right", "bottom"),
        frame = FALSE
      ) +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  observeEvent(input$YearLX, {
    
    #if(is.null(td)) return()
    ## get the choice from teh drop-down box
    YEAR = as.numeric(input$YearLX)
    
    ## supbset the data based on the choice
    if(YEAR != 2019){
       tmp_new<- rainfall_mean[rainfall_mean$Year == YEAR, ]
       tmp_new <- left_join(mpsz, tmp_new,
                        by = c("SUBZONE_N" = "SZ"))
    }else{
      tmp_new <- tmp
    }
    ## plot the subsetted ata
    output$mymap = renderLeaflet({
      tm <- tm_shape(tmp_new) +
        tm_fill(
          "mean_rain",
          style = "quantile",
          palette = "Blues",
          legend.hist = TRUE,
          legend.is.portrait = TRUE,
          legend.hist.z = 0.1
        ) +
        tm_layout(
          legend.height = 0.45,
          legend.width = 0.35,
          legend.outside = FALSE,
          legend.position = c("right", "bottom"),
          frame = FALSE
        ) +
        tm_borders(alpha = 0.5)
      tmap_leaflet(tm)
    })
  })
}
######################### 4. Finish app ##########################
shinyApp(ui,server)
