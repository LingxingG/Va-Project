####################### 1. define Package to install #######################
packages = c(
  'dplyr',
  'tidyverse',
  'sf', 
  'tmap',
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
mainDF <- read.csv("..\\merged data\\dataset.csv")

mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")

### 2.2 define dashboard elemets ###
header <- dashboardHeader(title = "Rain and Shiny Dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard1",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem(
    "Dashboard2",
    tabName = "widgets",
    icon = icon("dashboard")
  )
))

### 2.2.1 dfine dashboard body elements ###
dashboard1 <- tabItem(tabName = "dashboard",
                      fluidRow(box(plotOutput("plot1", height = 250)),
                               
                               box(
                                 title = "Controls",
                                 sliderInput("slider", "Number of observations:", 1, 100, 50)
                               )))

dashboard2 <- tabItem(tabName = "widgets",
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

ui <- dashboardPage(header, sidebar, body)


######################### 3. define input output ##########################
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

######################### 4. Finish app ##########################

shinyApp(ui, server)
