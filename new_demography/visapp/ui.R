##########################################################
## user interface
##########################################################  

ui<-fluidPage(theme = shinytheme("sandstone"),
              
              titlePanel("Visualizing Population Projection"),
              sidebarPanel(width = 3,
                           wellPanel(selectInput("cnt", "Select country:", 
                                                 choices = countries, selected = countries[1])),
                           wellPanel(selectInput("inc.type", "Scale-up type:", 
                                                 choices = c("fixed","varying"), selected = "fixed")),
                           wellPanel(sliderInput("inc.val", "Scale-up value:", 
                                                 min = 0, max = 0.9, step = 0.01, value = 0.1))),
              mainPanel(
                tabsetPanel(
                  tabPanel("Summary demography",
                           fluidRow(
                             column(width = 3, highchartOutput("linea")),
                             column(width = 3, highchartOutput("lineb")),
                             column(width = 3, highchartOutput("e0plot")),
                             column(width = 3, highchartOutput("birthb"))
                           ),
                           br(),
                           fluidRow(
                             column(width = 3, highchartOutput("lineq1")),
                             column(width = 3, highchartOutput("lineq5")),
                             column(width = 3, highchartOutput("lineq15")),
                             column(width = 3, highchartOutput("lineq30"))
                           ),
                           br(),
                           fluidRow(
                             column(width = 6, highchartOutput("poppy0")),
                             column(width = 6, highchartOutput("poppy1")))
                  )
                )
              ))