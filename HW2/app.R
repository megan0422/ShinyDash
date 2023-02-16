library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

#DATA MANIPULATION AND CLEANING 
food <- read_csv("food.csv", show_col_types = FALSE)
colnames(food)[colnames(food) == "Packging"] <- "Packaging" #Changing a misspelling
#Make a  new categorical variable for food product categories using case_when
food <- food %>%
  mutate(category = case_when(
    grepl("Beef|Lamb|Pig|Poultry|Fish", product, ignore.case = TRUE) ~ "Meat/Seafood",
    grepl("Potato|Tomato|Citrus|Banana|Apple|Berries|Onions|Root Vegetables|Brassicas", product, ignore.case = TRUE) ~ "Fruit/Vegetable",
    grepl("Dairy|Milk|Cheese|Eggs", product, ignore.case = TRUE) ~ "Dairy/Eggs",
    TRUE ~ "Grain/Nut/Seed")
  )%>%
  relocate(category, .after = product)

#USER INTERFACE SIDE
ui <- dashboardPage(
                dashboardHeader(title = "Environmental Impact of Food Production",
                                titleWidth = 400),
                dashboardSidebar(
                  sidebarMenu(width = 2,
                              id = "tabs",
                              
                              #Page tabs
                              menuItem("Home", icon = icon("home"), tabName = "home"),
                              menuItem("Bar chart", icon = icon("bar-chart"), tabName = "bar"),
                              menuItem("Pie chart", icon = icon("chart-pie"), tabName = "pie"),
                              
                              #Inputs and filters
                              selectInput("y", "Select an emissions variable for the y-axis of the histogram,
                                           bar chart, and pie chart breakdown:", 
                                           c("Land Use (Kg CO2)" = "Land_use",
                                             "Animal Feed (Kg CO2)" = "Animal_feed",
                                             "Farm (Kg CO2)" = "Farm",
                                             "Processing (Kg CO2)" = "Processing",
                                             "Transport (Kg CO2)" = "Transport",
                                             "Packaging (Kg CO2)" = "Packaging",
                                             "Retail (Kg CO2)" = "Retail",
                                             "Total Emissions (Kg CO2)" = "Total_emissions",
                                             "Eutrophying (per 100 kcal)" = "Eutrophying_emissions_kcal",
                                             "Eutrophying (per kilogram)" = "Eutrophying_emissions_kilogram",
                                             "Eutrophying (per 100g protein)" = "Eutrophying_emissions_protein",
                                             "Freshwater Withdrawals (per 100 kcal)" = "Freshwater_withdrawals_kcal",
                                             "Freshwater Withdrawals (per 100g protein)" = "Freshwater_withdrawals_protein",
                                             "Freshwater Withdrawals (per kilogram)" = "Freshwater_withdrawals_kilogram",
                                             "Greenhouse Gas (per 100 kcal)" = "Greenhouse_gas_kcal",
                                             "Greenhouse Gas (per 100g protein)" = "Greenhouse_gas_protein",
                                             "Land Use (per 100 kcal)" = "Land_use_kcal",
                                             "Land Use (per kilogram)" = "Land_use_kilogram",
                                             "Land Use (per 100g protein)" = "Land_use_protein",
                                             "Scarcity Weighted Water Use (per kilogram)" = "Scarcity_water_kilogram",
                                             "Scarcity Weighted Water Use (per 100g protein)" = "Scarcity_water_protein",
                                             "Scarcity Weighted Water Use (per 100 kcal)" = "Scarcity_water_kcal"),
                                           selected = "Total_emissions"),
                               selectInput("x", "Select an emissions variable for the x-axis of the histogram:", 
                                           c("Land Use (Kg CO2)" = "Land_use",
                                             "Animal Feed (Kg CO2)" = "Animal_feed",
                                             "Farm (Kg CO2)" = "Farm",
                                             "Processing (Kg CO2)" = "Processing",
                                             "Transport (Kg CO2)" = "Transport",
                                             "Packaging (Kg CO2)" = "Packaging",
                                             "Retail (Kg CO2)" = "Retail",
                                             "Total Emissions (Kg CO2)" = "Total_emissions",
                                             "Eutrophying (per 100 kcal)" = "Eutrophying_emissions_kcal",
                                             "Eutrophying (per kilogram)" = "Eutrophying_emissions_kilogram",
                                             "Eutrophying (per 100g protein)" = "Eutrophying_emissions_protein",
                                             "Freshwater Withdrawals (per 100 kcal)" = "Freshwater_withdrawals_kcal",
                                             "Freshwater Withdrawals (per 100g protein)" = "Freshwater_withdrawals_protein",
                                             "Freshwater Withdrawals (per kilogram)" = "Freshwater_withdrawals_kilogram",
                                             "Greenhouse Gas (per 100 kcal)" = "Greenhouse_gas_kcal",
                                             "Greenhouse Gas (per 100g protein)" = "Greenhouse_gas_protein",
                                             "Land Use (per 100 kcal)" = "Land_use_kcal",
                                             "Land Use (per kilogram)" = "Land_use_kilogram",
                                             "Land Use (per 100g protein)" = "Land_use_protein",
                                             "Scarcity Weighted Water Use (per kilogram)" = "Scarcity_water_kilogram",
                                             "Scarcity Weighted Water Use (per 100g protein)" = "Scarcity_water_protein",
                                             "Scarcity Weighted Water Use (per 100 kcal)" = "Scarcity_water_kcal"),
                                           selected = "Animal_feed"),
                               sliderInput("emissions", "Pick a range of total emissions to filter food products in the dataset
                                           (this will also change the histogram):",
                                           min = 0, max = 60, value = c(0,10)),
                               #show data table
                               checkboxInput(inputId = "show_data",
                                             label = "Show data table",
                                             value = TRUE))),
                  
                  #Output - tabs
                  dashboardBody(
                    tabItems(
                      tabItem("home",
                        fluidRow(
                          valueBoxOutput("total_emissions"),
                          valueBoxOutput("x_total"),
                          valueBoxOutput("y_total")
                          ),
                        fluidRow(
                          plotlyOutput(outputId = "scatterplot")
                          ),
                        fluidRow(
                          DT::dataTableOutput(outputId = "datatable")
                          )
                        ),
                      tabItem("bar",
                              fluidRow(
                                plotlyOutput(outputId = "barchart")
                              ),
                      tabItem("pie", 
                               fluidRow(
                                 plotlyOutput(outputId = "piechart")
                                 )
                              )
                      )
                      )
                    )
)

#SERVER SIDE
server <- function(input, output) {
  
  #REACTIVE 
  food_filtered <- reactive({
    req(input$y, input$x)
    food %>% 
      filter(Total_emissions >= input$emissions[1] & Total_emissions <= input$emissions[2]) %>% #filter based on the range on total emissions
      arrange(desc(!!sym(input$y))) #arrange by selected y (also what they will see on bar chart)
  })
  
  #Render the scatter plot 
  output$scatterplot <- renderPlotly({
    ggplotly(
      ggplot(data = food_filtered(), aes_string(x = input$x, y = input$y, color = "product")) +
        geom_point(size = 5) +
        labs(x = tools::toTitleCase(gsub("_", " ", input$x)),
             y = tools::toTitleCase(gsub("_", " ", input$y)),
             color = "Food Product"
        ) +
        theme_classic() +
        theme(legend.position = "bottom")
    )
  })
  
  # Render the bar chart
  output$barchart <- renderPlotly({
    ggplotly(
      ggplot(data = food_filtered(), aes_string(x = "category", y = input$y, fill = "category")) +
        geom_bar(stat = "identity") +
        xlab("Food Category") +
        ylab(tools::toTitleCase(gsub("_", " ", input$y))) +
        ggtitle("Contribution of Different Food Categories to Selected Emission Type") +
        scale_fill_brewer(palette = "Paired") +
        theme_classic() + 
        theme(legend.position = "none")
    )
  })
  
  # Render the pie chart
  output$piechart <- renderPlotly({
    fig <- plot_ly(food_filtered(), labels = ~category, values = ~Total_emissions, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'percent',
                   marker = list(colors = c('#c584e4', '#82ac64', '#00bbd4', '#fef769'),
                                 line = list(color = '#FFFFFF', width = 1)))
    fig <- fig %>% layout(title = 'Total Emissions by Food Category',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
  #Render data table on all tabs (if checked)
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = food_filtered(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
  
  #Render the value boxes 
  output$total_emissions <- renderValueBox({
    valueBox(
      value = format(round(aggregate(Total_emissions ~ category,
                                     data = food, sum)$Total_emissions), nsmall = 2), #this will aggregate total emissions by food category
      subtitle = "Total Emissions",
      icon = icon("earth-americas"),
      color = "blue"
    )
  })
  
  output$y_total <- renderValueBox({
    valueBox(
      value = format(round(sum(food$input[food$product == input$y]), nsmall = 2)),
      subtitle = tools::toTitleCase(gsub("_", " ", input$y)),
      icon = icon("fire"),
      color = "red"
    )
  })
  
  output$x_total <- renderValueBox({
    valueBox(
      value = format(round(sum(food$input[food$product == input$x]), nsmall = 2)),
      subtitle = tools::toTitleCase(gsub("_", " ", input$x)),
      icon = icon("tree"),
      color = "green"
    )
  })
  
}

#run the dashboard
shinyApp(ui = ui, server = server)