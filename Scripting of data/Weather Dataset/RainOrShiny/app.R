### define Package to install ###
packages = c(
  'dplyr',
  'tidyverse',
  'sf', 
  'tmap',
  'shinydashboard'
)

### loading all necessary packages ###
for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

############################ define dashboard UI ##########################
header <- dashboardHeader(title = "Rain and Shiny Dashboard")


#@@@@@@@@@@@@@@@@@@@@@@@@@@ To fill up @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

##################### declare main csv for DF #####################
mainDF <- read.csv("..\\merged data\\dataset.csv")

mpsz <- st_read(dsn = "geospatial",
                layer = "MP14_SUBZONE_WEB_PL")

#####################################################################

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
##############################################################################

##################### Just fill in dashboard variable ########################
body <- dashboardBody(dashboardBody(tabItems(
  
  # First tab content
  dashboard1,
  
  # Second tab content
  dashboard2)))

ui <- dashboardPage(header, sidebar, body)
##############################################################################


################################ To FILL UP ##################
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}
#############################################################################

shinyApp(ui, server)
