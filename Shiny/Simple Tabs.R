library("shiny")
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deterministic", tabName = "determ", icon = icon("balance-scale")),
      menuItem("Single Draw", tabName = "rand_draw", icon = icon("dice")),
      menuItem("Montecarlo Microimulation", tabName = "CEAC", icon = icon("chart-area")),
      menuItem("Randomised Willingness to Pay", tabName = "portion", icon = icon("dice-d20"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("rand_draw",
              box(
                title = "Incremental Cost-Effectiveness Ratio (Single Draw from a Distribution)",
                background = "fuchsia",
                width = 4,
                textOutput("ICER")
              ),
              box(
                numericInput("int_cost", em("Expected Healthcare Cost with Intervention:"), 500, min = 0, max = 10000), width = 4
              ),
              box(
                numericInput("bau_cost", em("Expected Healthcare Cost without Intervention:"), 100, min = 0, max = 10000), width = 4
              )       
      ),
      tabItem("determ"),
      tabItem("CEAC",
              box(
                title = "Cost-Effectiveness Acceptability Curve",
                background = "teal",
                width = 12,
                plotOutput("CEAC")
                ),
              box(
                numericInput("CEAC_int_cost", em("Expected Healthcare Cost with Intervention:"), 500, min = 0, max = 10000), width = 6)  
              )
              ),
      tabItem("portion")
    )
  ) 


server <- function(input, output){
  
}

shinyApp(ui,server)