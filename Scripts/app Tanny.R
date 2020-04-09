####################### 1. define Package to install #######################
packages = c(
  'dplyr',
  'tidyverse',
  'sf',
  'tmap',
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
  'leaflet',
  "shinythemes",
  "ggrepel",
  "forcats",
  "shinydashboard"
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
                      fillPage(titlePanel("Ridge Plot"),
                               fluidRow(
                                 column(4, tmapOutput("lxmap")),
                                 column(4, tmapOutput("lxmap2"))
                               ),
                               br(),
                               fluidRow(
                                 column(4, uiOutput("sYear")),
                                 column(4, uiOutput("sMonth"))
                               )))


dashboard2 <- tabItem(tabName = "dashboard2",
                      fluidPage(titlePanel("Ridge Plot"),
                                fluidRow(
                                  uiOutput("tYear1"),
                                  fluidRow(plotlyOutput("tanny1"))
                                )))

dashboard3 <- tabItem(tabName = "dashboard3",
                      fluidPage(titlePanel("Ridge Plot"),
                                fluidRow(uiOutput("tYear3"),
                                         fluidRow(
                                           column(6, plotOutput("tanny2",width="100%",height="400px")),
                                           column(6, plotOutput("tanny3",width="100%",height="400px"))
                                         ))))

dashboard4 <- tabItem(tabName = "dashboard4",
                      fluidPage(
                        fluidRow(),
                        fluidRow(uiOutput("tYear")),
                        fluidRow(plotlyOutput("tanny4")),
                        
                      ))
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
server <- function(input, output, session) {
  #----------------------------------------dashboard 4---------------------------------------
  output$tYear <- renderUI({
    Year_min <- min(rainfall[, "Year"], na.rm = TRUE)
    Year_max <- max(rainfall[, "Year"], na.rm = TRUE)
    sliderInput(
      inputId = "YearTanny4",
      label = "Year:",
      min = 2009,
      max = Year_max,
      value = c(2009),
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
               x = round(mean_rain, 2),
               y = round(mean_temp, 2),
               color = Month
             )) +
      geom_point(aes(text=map(paste('<b>letter:</b>', mean_rain, '<br/>', '<b>Letter:</b>', mean_temp), HTML))) +
      theme(legend.position = "none",
            legend.title = element_blank()) +
      labs(y = " Mean Temperature (\u00B0C)", x = "Rain Precipitation (mm)")
    
    db4scatter <-
      ggplotly(
        scatterPlot 
        # + geom_label_repel(
        #   aes(label = Month),
        #   box.padding   = 0.35,
        #   point.padding = 0.5,
        #   segment.color = 'grey50'
        # )
      )
    
    raindensity <-
      ggplot(tanny4(), aes(mean_rain, fill = '#E69F00')) +
      geom_density(alpha = .5) +
      scale_fill_manual(values = c('#E69F00')) +
      theme(legend.position = "none")
    
    db4rain <- ggplotly(raindensity)
    
    tempdensity <-
      ggplot(tanny4(), aes(x = mean_temp, fill = '#E69F00')) +
      geom_density(alpha = .5) +
      scale_fill_manual(values = c('#999999')) +
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
  #----------------------------------------dashboard 3---------------------------------------
  output$tYear3 <- renderUI({
    Year_max <- max(temperature[, "Year"], na.rm = TRUE)
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
           subtitle = 'Mean temperatures (Celcius) by month') +
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
           subtitle = 'Mean Rainfall Precipitation (mm) by month') +
      theme_ridges(font_size = 13, grid = TRUE) +
      theme(axis.title.y = element_blank())
  })
  
  #----------------------------------------dashboard 2---------------------------------------
  output$tYear1 <- renderUI({
    Year_max <- max(temperature[, "Year"], na.rm = TRUE)
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
    rain <- ggplot(na.omit(masterDF), aes(factor(Month), mean_rain))+
            geom_violin(color = "#B2BCC2", add = "boxplot", fill=NA) +
            geom_boxplot(width = 0.1,
                         fill = "#4863A0",
                         alpha = 0.2)+
            ggtitle ("Rainfall distribution by month") + 
            theme(legend.position="none") +
            xlab("Month") + 
            ylab("Average Rainfall ( mm )")
      
    rain <- ggplotly(rain)
  })
  
  #----------------------------------------dashboard 1----------------------------------------
  # render filters in UI 
  output$sYear <- renderUI({
    Year_min <- min(rainfall[, "Year"], na.rm = TRUE)
    Year_max <- max(rainfall[, "Year"], na.rm = TRUE)
    sliderInput(
      inputId = "YearLX",
      label = "Year:",
      min = 2000,
      max = Year_max,
      value = c(2009),
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
    tmp2 <- masterDF %>%
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
}
######################### 4. Finish app ##########################
shinyApp(ui,server)