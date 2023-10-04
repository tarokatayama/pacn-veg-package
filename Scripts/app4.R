# Load the Shiny library
library(shiny)

# Define the user interface (UI)
ui <- fluidPage(
  # App title
  titlePanel("Monthly Utilities Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select a month
      selectInput("month", "Select a Month:",
                  choices = c(
                    "August", "September", "October", "November",
                    "December", "January", "February", "March"
                  )),
      
      # Input fields for utility costs
      numericInput("electricity", "Electricity Cost ($):", value = 0),
      numericInput("water", "Water Cost ($):", value = 0),
      numericInput("power", "Power Cost ($):", value = 0),
      numericInput("internet", "Internet Cost ($):", value = 0),
      
      # Calculate button
      actionButton("calculate", "Add bc u can't math")
    ),
    
    mainPanel(
      # Output to display the total monthly cost
      h4("Total Monthly Cost:"),
      textOutput("total_cost"),
      
      # Outputs to display individual monthly costs for each user
      h4("Monthly Cost per User:"),
      textOutput("taro_cost"),
      textOutput("sheryl_cost"),
      textOutput("edward_cost")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Calculate the total cost
  total_cost <- reactive({
    electricity_cost <- input$electricity
    water_cost <- input$water
    power_cost <- input$power
    internet_cost <- input$internet
    
    total <- electricity_cost + water_cost + power_cost + internet_cost
    return(total)
  })
  
  # Calculate the individual monthly cost per user (dividing by 3)
  taro_cost <- reactive({
    total_cost() / 3
  })
  sheryl_cost <- reactive({
    total_cost() / 3
  })
  edward_cost <- reactive({
    total_cost() / 3
  })
  
  # Render the total cost
  output$total_cost <- renderText({
    paste("$", total_cost())
  })
  
  # Render the individual monthly costs
  output$taro_cost <- renderText({
    paste("Taro: $", taro_cost())
  })
  output$sheryl_cost <- renderText({
    paste("Sheryl: $", sheryl_cost())
  })
  output$edward_cost <- renderText({
    paste("Edward: $", edward_cost())
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
