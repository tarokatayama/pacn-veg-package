# Load the Shiny library
#install.packages("shiny")
library(shiny)

# Define the user interface (UI)
ui <- fluidPage(
  # App title
  titlePanel("Monthly Utilities Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input fields for each utility
      numericInput("electricity", "Electricity Cost ($):", value = 0),
      numericInput("water", "Water Cost ($):", value = 0),
      numericInput("gas", "Gas Cost ($):", value = 0),
      
      # Calculate button
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      # Output to display the total monthly cost
      h4("Total Monthly Cost:"),
      textOutput("total_cost")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Calculate the total cost
  total_cost <- reactive({
    electricity_cost <- input$electricity
    water_cost <- input$water
    gas_cost <- input$gas
    
    total <- electricity_cost + water_cost + gas_cost
    return(total)
  })
  
  # Render the total cost
  output$total_cost <- renderText({
    paste("$", total_cost())
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
