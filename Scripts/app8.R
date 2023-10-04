# Load the Shiny, plotly, and DT libraries
library(shiny)
library(plotly)
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
      
      # Add a button to add the current month's data
      actionButton("add_month", "Add Current Month"),
      
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
server <- function(input, output, session) {
  # Initialize an empty data frame to store utility data for all months
  all_data <- reactiveVal(data.frame(Month = character(0),
                                     Electricity = numeric(0),
                                     Water = numeric(0),
                                     Power = numeric(0),
                                     Internet = numeric(0)))
  
  # Add the current month's data when the "Add Current Month" button is clicked
  observeEvent(input$add_month, {
    month <- input$month
    new_data <- data.frame(
      Month = month,
      Electricity = input$electricity,
      Water = input$water,
      Power = input$power,
      Internet = input$internet
    )
    current_data <- all_data()
    new_data <- rbind(current_data, new_data)
    all_data(new_data)
  })
  
  # Calculate the total cost
  total_cost <- reactive({
    sum(all_data()[, 2:5])
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
  
  # Create a stacked bar chart for monthly utilities
  output$utilities_chart <- renderPlotly({
    data_long <- reshape2::melt(all_data(), id.vars = "Month")
    
    plot <- plot_ly(data_long, x = ~Month, y = ~value, type = "bar", name = ~variable, split = ~variable) %>%
      layout(title = "Monthly Utilities Breakdown",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Cost ($)"),
             barmode = "stack")
    
    return(plot)
  })
  
  # Create a table to display the monthly utilities breakdown
  output$utilities_table <- renderDT({
    # Calculate the sum of each utility for each month
    utility_sums <- aggregate(. ~ Month, all_data(), sum)
    
    # Bind the utility sums to the original data
    all_data_with_sums <- merge(all_data(), utility_sums, by = "Month", all.x = TRUE)
    
    # Create a table with only one column for each utility
    table_data <- all_data_with_sums[, c("Month", "Electricity.x", "Water.x", "Power.x", "Internet.x")]
    
    # Rename the columns
    colnames(table_data) <- c("Month", "Electricity", "Water", "Power", "Internet")
    
    # Add a total column for each month
    table_data$Total <- rowSums(table_data[, 2:5])
    
    datatable(table_data, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
  })
  
  # Render the monthly cost per user
  output$taro_cost <- renderText({
    paste("Taro's Monthly Cost: $", round(taro_cost(), 2))
  })
  output$sheryl_cost <- renderText({
    paste("Sheryl's Monthly Cost: $", round(sheryl_cost(), 2))
  })
  output$edward_cost <- renderText({
    paste("Edward's Monthly Cost: $", round(edward_cost(), 2))
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

