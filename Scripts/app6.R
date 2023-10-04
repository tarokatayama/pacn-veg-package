# Load the Shiny, plotly, and DT libraries
library(shiny)
library(plotly)
#install.packages("DT")
library(DT)

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
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      # Output to display the total monthly cost
      h4("Total Monthly Cost:"),
      textOutput("total_cost"),
      
      # Stacked bar chart to visualize monthly utilities
      plotlyOutput("utilities_chart"),
      
      # Output to display the breakdown table
      h4("Monthly Utilities Breakdown:"),
      DTOutput("utilities_table"),
      
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
  
  # Create a stacked bar chart for monthly utilities
  output$utilities_chart <- renderPlotly({
    data <- data.frame(
      Month = input$month,
      Electricity = input$electricity,
      Water = input$water,
      Power = input$power,
      Internet = input$internet
    )
    
    data_long <- reshape2::melt(data, id.vars = "Month")
    
    plot <- plot_ly(data_long, x = ~Month, y = ~value, type = "bar", name = ~variable) %>%
      layout(title = "Monthly Utilities Breakdown",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Cost ($)"),
             barmode = "stack")
    
    return(plot)
  })
  
  # Create a table to display the monthly utilities breakdown
  output$utilities_table <- renderDT({
    data <- data.frame(
      Month = input$month,
      Electricity = input$electricity,
      Water = input$water,
      Power = input$power,
      Internet = input$internet,
      Total = total_cost()
    )
    
    datatable(data, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
