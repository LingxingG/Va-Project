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
  "shinydashboard",
  'waiter',
  'highcharter',
  'shinycssloaders'
)

for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

################### 1.1 CSS ##########################
css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'Loading ...'; }
}
"
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
    ),
    menuItem(
      "Weathers Radials",
      tabName = "dashboard5",
      icon = icon("dashboard")
    )
  )
)

### 2.1.1 dfine dashboard body elements ###
dashboard1 <- tabItem(tabName = "dashboard1",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Ridge Plot"),
                        fluidRow(column(4, tmapOutput("lxmap")),
                                 column(4, tmapOutput("lxmap2"))),
                        br(),
                        fluidRow(column(4, uiOutput("sYear")),
                                 column(4, uiOutput("sMonth")))
                      ))

dashboard2 <- tabItem(tabName = "dashboard2",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Ridge Plot"),
                        fluidRow(uiOutput("tYear1"),
                                 fluidRow(plotOutput("tanny1")))
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
                        titlePanel("Ridge Plot"),
                        fluidRow(),
                        fluidRow(uiOutput("tYear")),
                        fluidRow(plotlyOutput("tanny4")),
                      ))

dashboard5 <- tabItem(tabName = "dashboard5",
                      fluidPage(
                        tags$style(type = "text/css", css),
                        titlePanel("Weathers Radials"),
                        fluidRow(column(4,     
                                        sliderInput(
                                          inputId = "hc_Year",
                                          label = "Year:",
                                          min = 1982,
                                          max = 2019,
                                          value = 1982,
                                          step = 1,
                                          sep = "",
                                          animate = animationOptions(interval = 5000,
                                                                     loop = FALSE))
                                        ),
                                 column(4, uiOutput('hcRegion'))),
                        fluidRow(column(6, withSpinner(
                          highchartOutput("hc", width = "100%", height = "600px")
                        )))
                      ))

### 2.1.2 Fill in dashboard elements ####
body <- dashboardBody(
  use_waiter(),
  waiter_show_on_load(tagList(spin_fading_circles(),
                              "Loading ...")),
  tabItems(
    dashboard1,
    dashboard2,
    dashboard3,
    dashboard4,
    dashboard5
  )
)

ui <- dashboardPage(header, sidebar, body)

######################### 3. define input output ##########################
### 3.1 import attribute data ###
mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")
mainDF <- read.csv("merged data\\dataset.csv")
mainDF$date <- as.Date(with(mainDF, paste(Year, Month, Day,sep="-")), "%Y-%b-%d")

rainfall3 <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Daily Rainfall Total")) %>%
  group_by(Region, SZ, Station,Year, Month) %>%
  summarise(mean_rain = mean(Value, na.rm = TRUE))


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

#--------------------------------Weathers Radials-------------------------------------
Mastertemp <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Mean Temperature")) %>%
  select(Region,Year, date, Value) %>%
  group_by(Year,Region,date) %>%
  summarise(mean_temperaturec  = mean(Value, na.rm = TRUE))

temperature_max <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Maximum Temperature")) %>%
  select(Region,Year, date, Value) %>%
  group_by(Year,Region,date) %>%
  summarise(max_temperaturec = mean(Value, na.rm = TRUE))

temperature_min <- mainDF %>%
  filter(str_detect(mainDF$Measurement, "Minimum Temperature")) %>%
  select(Region, Year, date, Value) %>%
  group_by(Year, Region, date) %>%
  summarise(min_temperaturec = mean(Value, na.rm = TRUE))

Mastertemp$max_temperaturec = temperature_max$max_temperaturec
Mastertemp$min_temperaturec = temperature_min$min_temperaturec

Mastertemp <- Mastertemp %>%
  na.omit()
Year_min <- min(Mastertemp[, "Year"], na.rm = TRUE)
Year_max <- max(Mastertemp[, "Year"], na.rm = TRUE)
server <- function(input, output, session) {
  
  #----------------------------------------dashboard 5---------------------------------------
  output$hcRegion <- renderUI({
    
    tmp <- Mastertemp %>%
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
      
    Mastertemp <- Mastertemp %>%
      filter(Region == as.character(input$hc_Region)) %>%
      filter(Year == as.numeric(input$hc_Year))
    
    hchart(Mastertemp, type = "columnrange",
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
  output$tanny1 <- renderPlot({
    rain <- ggplot(na.omit(masterDF), aes(factor(Month), mean_rain, fill =factor(Month)))
    rain + geom_violin(fill = "lightblue") +
      geom_boxplot(width = 0.1,
                   color = "white",
                   alpha = 0.2)+
      ggtitle ("Rainfall distribution by month") + 
      theme(legend.position="none") +
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
      min = 2009,
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
  waiter_hide()
}
######################### 4. Finish app ##########################
shinyApp(ui,server)