library(shiny)




# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Portuguese Soccer"),
  sidebarPanel(
    selectInput("pickLeague", "League", c("England", "Portugal"), selected = "Portugal", multiple = FALSE),
    sliderInput("years","Seasons",min = 1994, max = 2016, value = c(1994, 2016), ticks = FALSE, sep=""),
    checkboxGroupInput("teamNames", "Pick the Teams", c(unique(totalTeams)), selected = "Porto")),
  
  mainPanel(
    plotOutput("plot1", click = "plot_click"))
  
)



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    load("por.RData")
    paste("You have selected\n", input$teamNames)
    subT <- totalTable[totalTable$Season %in% input$years[1]:input$years[2],]
    subT <- subT[subT$Team %in% input$teamNames,]
    ggplot(data=subT, aes(x=Season, y=Wins, group =Team, colour = factor(Team))) +
      geom_line()+
      geom_point()
  })
  
  
  
}

shinyApp(ui, server)