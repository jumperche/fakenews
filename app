library(shiny)

# UI-Definition
ui <- fluidPage(
  titlePanel("Fake News Opinion Change Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("nInput", "Number of Members", value = 100000),
      sliderInput("friendsInput", "Number of Friends", min = 2, max = 100, value = 15),
      sliderInput("colleaguesPercentInput", "Percentage of Friends that are Colleagues", min = 1, max = 100, value = 1),
      sliderInput("ageRangeInput", "Age Range", min = 12, max = 100, value = c(12, 100)),
      sliderInput("percentReadInput", "Percentage who Read the Fake News", min = 0, max = 100, value = 80),
      sliderInput("percentShareInput", "Percentage who Share the Fake News", min = 0, max = 100, value = 50),
      numericInput("roundsInput", "Number of Simulation Rounds", value = 1, min = 1),
      actionButton("runSimulation", "Run Simulation")
    ),
    
    mainPanel(
      textOutput("result"),
      tableOutput("roundsResult")
    )
  )
)

# Server-Logik
server <- function(input, output) {
  results <- reactiveValues(data = NULL)
  
  observeEvent(input$runSimulation, {
    isolate({
      n <- input$nInput
      friends <- input$friendsInput
      colleaguesPercent <- input$colleaguesPercentInput / 100
      ageRange <- input$ageRangeInput
      percentRead <- input$percentReadInput / 100
      percentShare <- input$percentShareInput / 100
      rounds <- input$roundsInput
      
      roundResults <- numeric(rounds)
      
      for (round in 1:rounds) {
        # Datenmatrix erstellen
        data <- data.frame(
          ID = 1:n,
          Number_of_Friends = sample(friends:friends, n, replace = TRUE),
          Number_of_Colleagues = pmax(floor(sample(friends:friends, n, replace = TRUE) * colleaguesPercent), 2),
          Age = sample(ageRange[1]:ageRange[2], n, replace = TRUE),
          Gender = rep(c(0, 1), each = n/2),
          Opinion = rep("Unchanged", n)
        )
        
        # Matrix für das Lesen und Teilen von Fake News
        read_shared <- matrix(FALSE, nrow = n, ncol = 2)
        colnames(read_shared) <- c("Read", "Shared")
        
        # Fake News Simulation
        for (id in 1:n) {
          if (runif(1) < percentRead) {
            read_shared[id, "Read"] <- TRUE
            if (runif(1) < percentShare) {
              read_shared[id, "Shared"] <- TRUE
            }
          }
        }
        
        # Meinungsänderungen berechnen
        for (id in 1:n) {
          if (read_shared[id, "Read"] && read_shared[id, "Shared"]) {
            if ((data$Gender[id] == 0 && data$Age[id] < 40) || (data$Gender[id] == 1 && data$Age[id] < 50)) {
              data$Opinion[id] <- "Changed"
            }
          }
        }
        
        # Ergebnisse der aktuellen Runde speichern
        roundResults[round] <- sum(data$Opinion == "Changed")
      }
      
      results$data <- roundResults
    })
  })
  
  output$result <- renderText({
    if (!is.null(results$data)) {
      totalChanges <- sum(results$data)
      paste("Total Number of Opinion Changes Across All Rounds:", totalChanges)
    }
  })
  
  output$roundsResult <- renderTable({
    if (!is.null(results$data)) {
      data.frame(Round = 1:length(results$data), Opinion_Changes = results$data)
    }
  })
}

# App starten
shinyApp(ui = ui, server = server)
