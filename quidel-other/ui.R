
require(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Quidel"),
  sidebarPanel(
    sliderInput("date", "Week to be displayed:", 
                min=1, max= 69,value = 1,step = 1,
                format="###0",animate=TRUE)),
  mainPanel(
    h3(textOutput("date")), 
    htmlOutput("gvis"))
)
)