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
  'ggstatsplot',
  'ggplot2',
  'gganimate',
  'ggridges',
  "gridExtra",
  "htmlwidgets",
  "shinythemes",
  "forcats"
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
      "Dashboard 1",
      tabName = "dashboard1",
      icon = icon("dashboard")
    ),
    menuItem(
      "Dashboard 2",
      tabName = "dashboard2",
      icon = icon("dashboard")
    ),
    menuItem(
      "Dashboard 3",
      tabName = "dashboard3",
      icon = icon("dashboard")
    ),
    menuItem(
      "Dashboard 4",
      tabName = "dashboard4",
      icon = icon("dashboard")
    )
  )
)

### 2.1.1 dfine dashboard body elements ###
dashboard1 <- tabItem(tabName = "dashboard1",
                      fillPage(theme = shinytheme("united"),
                               title = "Tmap",
                                fluidRow(
                                  column(4, leafletOutput("lxmap")),
                                  column(4, leafletOutput("lxmap2"))
                                ),
                                br(),
                                fluidRow(
                                  column(4, uiOutput("sYear")),
                                  column(4, uiOutput("sMonth"))
                                )))

dashboard2 <- tabItem(tabName = "dashboard2",
                      fluidPage(theme = shinytheme("united"),
                                title = "mmap",
                                fluidRow(plotOutput("tanny1")
                               )
                               ))

dashboard3 <- tabItem(tabName = "dashboard3",
                      fluidPage(theme = shinytheme("united"),
                                title = "mmap",
                               fluidRow(
                                 column(4, plotOutput("tanny2")),
                                 column(4,plotOutput("tanny3"))
                               )))

dashboard4 <- tabItem(tabName = "dashboard4",
                      fluidPage(theme = shinytheme("united"),
                                title = "mmap",
                                fluidRow(
                                  plotOutput("tanny4")
                                )))


### 2.1.2 Fill in dashboard elements ####
body <- dashboardBody(tabItems(# First tab content
  dashboard1,
  dashboard2,
  dashboard3,
  dashboard4))

ui <- dashboardPage(header, sidebar, body, skin="black")

######################### 3. define input output ##########################
mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")
mainDF <- read.csv("merged data\\dataset.csv")

# declare base dataframe to use for both rain and temp
rainfall <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
  group_by(Region, SZ, Station,Year, Month) %>%
  summarise(mean_rain = mean(Value, na.rm = TRUE))

temperature <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
  group_by(Region, SZ, Station, Year, Month) %>%
  summarise(mean_temp = mean(Value, na.rm = TRUE))

masterDF <- rainfall %>%
  mutate(Month = fct_relevel(Month, 
                             "Jan","Feb","Mar",
                             "Apr","May","Jun",
                             "Jul","Aug","Sep",
                             "Oct","Nov","Dec"))
masterDF$mean_temp = temperature$mean_temp

######################### 3.1 define customer function ##########################

### 3.1 import attribute data ###
server <- function(input, output) {
  #----------------------------------------dashboard 4---------------------------------------
  output$tanny4 <- renderPlot({
    scatterPlot <- ggplot(masterDF,aes(x= mean_rain, y= mean_temp, color='#E69F00')) + 
      geom_point() + 
      scale_color_manual(values = c('#999999')) + 
      theme(legend.position=c(0,1), legend.justification=c(0,1))

    xdensity <- ggplot(masterDF, aes(x= mean_rain, fill='#E69F00')) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = c('#E69F00')) + 
      theme(legend.position = "none")
    # xdensity
    
    ydensity <- ggplot(masterDF, aes(y=mean_temp, fill='#E69F00')) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = c('#999999','#E69F00')) + 
      theme(legend.position = "none")
    # ydensity
    
    blankPlot <- ggplot()+geom_blank(aes(1,1))+
      theme(plot.background = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
      )
    
    grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
                 ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
  })
  #----------------------------------------dashboard 3---------------------------------------
  output$tanny2 <- renderPlot({
    temp <- ggplot(temperature, aes(x = mean_temp, y = Month, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Temp. [C]", option = "B") +
      labs(
        title = 'Temperatures in Lincoln NE',
        subtitle = 'Mean temperatures (Celcius) by month'
      ) +
      theme_ridges(font_size = 13, grid = TRUE) + 
      theme(axis.title.y = element_blank())
    tmp <- temperature %>% 
      filter(Year == 2018)
    
    temp+xlim(min(tmp$mean_temp, na.rm=TRUE)-1,max(tmp$mean_temp, na.rm=TRUE)+1)
  })
  
  output$tanny3 <- renderPlot({
    rf <- ggplot(rainfall, aes(x = mean_rain, y = factor(Month), fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Rain Precipitation (mm)", option = "D") +
      labs(
        title = 'Temperatures in Lincoln NE',
        subtitle = 'Mean Rainfall Precipiration (mm) by month'
      ) +
      theme_ridges(font_size = 13, grid = TRUE) + 
      theme(axis.title.y = element_blank())
    
    tmp <- rainfall %>% 
      filter(Year == 2018)
    
    rf+xlim(0,max(tmp$mean_rain, na.rm=TRUE)+5)
  })
  #----------------------------------------dashboard 2---------------------------------------
  output$tanny1 <- renderPlot({
    rain <- ggplot(na.omit(masterDF), aes(factor(Month), mean_rain))
    rain + geom_violin(fill = "lightblue") +
      geom_boxplot(width = 0.1,
                   color = "white",
                   alpha = 0.2)+
      ggtitle ("Rainfall distribution by month") + 
      xlab("Month") + 
      ylab("Average Rainfall ( mm )")
  })
  #----------------------------------------dashboard 1----------------------------------------
  
  # render filters in UI 
  output$sYear <- renderUI({
    Year_min <- min(rainfall[, "Year"], na.rm = TRUE)
    Year_max <- max(rainfall[, "Year"], na.rm = TRUE)
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
    Month_choices <- unique(masterDF$Month)
    selectInput(
      inputId = "MonthLX",
      label = "Month:",
      choices = Month_choices,
      selected = "Jan"
    )
  })
  
  ## define default variable for rain map
  tmp <- masterDF %>%
    filter(Year == 2019) %>%
    filter(Month == 'Jan')
  tmp <- left_join(mpsz, tmp,
                   by = c("SUBZONE_N" = "SZ")) %>%
         st_transform(4326)
  
  # show rain map
  output$lxmap = renderLeaflet({
    tm <- tm_shape(tmp)+
      tm_fill("mean_rain",
              style = "quantile",
              palette = "Blues") +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  # show temp map
  output$lxmap2 = renderLeaflet({
    tm <- tm_shape(tmp)+
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
       tmp_new<- masterDF %>%
         filter(Year == YEAR) %>%
         filter(Month == MONTH)
        
       tmp_new <- left_join(mpsz, tmp_new,
                            by = c("SUBZONE_N" = "SZ")) %>%
                  st_transform(4326)
      
    }else{
      tmp_new <- tmp
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
      tm <- tm_shape(tmp_new)+
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