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

      states <-c()
      edge.labels2 <-c()
      node.types2 <- c()
      Nodes <- list()
      nNodes <- c()
      zVal <- 0
      for(i in lines){
        if(grepl("[A-Z]",substr(i,1,1)) && grepl("[A-Z]",substr(i,nchar(i),nchar(i))) && grepl("[a-z]",substr(i,nchar(i)-1,nchar(i)-1))){
          firstState <- substr(i,1,1)
          transitionValue <- substr(i,nchar(i)-1,nchar(i)-1)
          lastState <- substr(i,nchar(i),nchar(i))
        }
        if(grepl("[A-Z]",substr(i,1,1)) && grepl("[a-z]",substr(i,nchar(i),nchar(i)))){
          firstState <- substr(i,1,1)
          transitionValue <- substr(i,nchar(i),nchar(i))
          lastState <- "Z"
        }
        
        states <- c(states,firstState,lastState)
        print(states)
        edge.labels2 <- c(edge.labels2,transitionValue)
        print(edge.labels2)
        
        if(!substr(i,1,1) %in% Nodes){
          Nodes <- c(Nodes, substr(i,1,1))
        }
        if(!substr(i,nchar(i),nchar(i)) %in% Nodes){
          Nodes <- c(Nodes, substr(i,nchar(i),nchar(i)))
        }
      }

      for(i in seq_along(states)){
        print(i)
        if(states[i] == "Z"){
          zVal <- i
          break
        }
      }

      #zVal <- zVal/2
      
      nNodes <- c(rep(2,length(Nodes)-1))
      node.types2 <-c(1,nNodes)
      
      if(zVal/2 <= length(node.types2)){
        node.types2[zVal/2] <- 3
      }
      print(node.types2)
      
      automaton2 <- graph(states, directed = T)
      
      mapping.colors <- c("green","gray", "red")
      node.colors <- mapping.colors[node.types2]
      
      plot(automaton2, edge.label = edge.labels2, vertex.color = node.colors)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

