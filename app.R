#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(igraph)
library(visNetwork)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regular Grammar to Automaton"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("text", "Enter your grammar to convert into an Automaton"),
        ),
        # Show a plot of the generated distribution
        mainPanel(
           uiOutput("inputText"),
           plotOutput("Converter")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$inputText <- renderUI({
      HTML(paste("<pre>", input$text, "</pre>", sep = "\n"))
    })
    
    output$Converter <- renderPlot({
      lines <- strsplit(input$text, "\n")[[1]]
      for(i in lines){
        
    }
    x <- c("apple", "banana", "pear")
    grepl("an",x)
        
      automaton <- graph(c("S","A", "S","B", "A","B", "A","Z", "B","Z"), directed = F)
      edge.labels <- c("a", "b", "b", "c", "c")
      node.types <- c(1,2,2,3)
      
      mapping.colors <- c("green", "gray", "red")
      node.colors <- mapping.colors[node.types]
      
      plot(automaton, edge.label = edge.labels, vertex.color = node.colors)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
