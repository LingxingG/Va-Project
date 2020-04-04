####################### 1. define Package to install #######################
packages = c(
  'dplyr',
  'tidyverse',
  'sf', 
  'tmap',
  'plotly',
  'shinydashboard'
)

for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

######################### 2. define dashboard UI ##########################

### 2.1 import attribute data ###
mainDF <- read.csv("merged data\\dataset.csv")

mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")

mpsz_mainDF <- left_join(mpsz, mainDF, 
                          by = c("SUBZONE_N" = "subzone_name"))

### 2.2 define dashboard elemets ###
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

### 2.2.1 dfine dashboard body elements ###
dashboard1 <- tabItem(tabName = "dashboard",
                      fluidRow(
                        plotOutput("plot1", height="550px")
                        )
                      )

dashboard2 <- tabItem(tabName = "dashboard2",
                      fluidRow(
                        box(title = "Box title", "Box content"),
                        box(status = "warning", "Box content")
                      ),
                      
                      fluidRow(
                        box(
                          title = "Title 1",
                          width = 4,
                          solidHeader = TRUE,
                          status = "primary",
                          "Box content"
                        ),
                        box(
                          title = "Title 2",
                          width = 4,
                          solidHeader = TRUE,
                          "Box content"
                        ),
                        box(
                          title = "Title 1",
                          width = 4,
                          solidHeader = TRUE,
                          status = "warning",
                          "Box content"
                        )
                      ),
                      
                      fluidRow(
                        box(
                          width = 4,
                          background = "black",
                          "A box with a solid black background"
                        ),
                        box(
                          title = "Title 5",
                          width = 4,
                          background = "light-blue",
                          "A box with a solid light-blue background"
                        ),
                        box(
                          title = "Title 6",
                          width = 4,
                          background = "maroon",
                          "A box with a solid maroon background"
                        )
                      ))

### 2.2.2 Fill in dashboard elements ####
body <- dashboardBody(dashboardBody(tabItems(
  
  # First tab content
  dashboard1,
  
  # Second tab content
  dashboard2)))

ui <- dashboardPage(header, sidebar, body, skin="black")


######################### 3. define input output ##########################
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    graph1 <- ggplot(tm_shape(mpsz_mainDF))
    
    ggplotly(graph1)
  })
}

######################### 4. Finish app ##########################
shinyApp(ui, server)
