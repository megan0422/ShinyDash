library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

#DATA MANIPULATION AND CLEANING 
food <- read_csv("food.csv", show_col_types = FALSE)
colnames(food)[colnames(food) == "Packging"] <- "Packaging" #Changing a misspelling
#Make a  new categorical vvariable for food product categories
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
                dashboardHeader(title = "Environmental Impact of Food Production"),
                dashboardSidebar(
                  sidebarPanel(width = 2,
                               selectInput("y", "Select a variable for the y-axis:", 
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
                                           selected = "Land Use (Kg CO2)"),
                               selectInput("x", "Select a variable for the x-axis:", 
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
                                           selected = "Greenhouse Gas Emissions per 100 kcal"),
                               sliderInput("emissions", "Pick a range of total emissions to filter food products for the dot plot:",
                                           min = 0, max = 60, value = c(0,10)),
                               #show data table
                               checkboxInput(inputId = "show_data",
                                             label = "Show data table",
                                             value = TRUE),
                  
                  #Output - creating two tabs
                  dashboardBody(
                    tabItems(
                      tabItem("Interactive Charts", 
                               fluidRow(
                                 column(6, plotOutput(outputId = "scatterplot")),
                                 column(6, plotOutput(outputId = "barchart"))),
                               fluidRow(
                                 DT::dataTableOutput(outputId = "datatable")
                               )),
                      tabItem("Pie Chart of Total Emissions", 
                               fluidRow(plotOutput(outputId = "piechart")),
                               fluidRow(
                                 DT::dataTableOutput(outputId = "piedata"))))
                )
)
)
)

#SERVER side
server <- function(input, output) {
  food_filtered <- reactive({
    req(input$y, input$x, input$emissions)
    food %>% 
      filter(Total_emissions >= input$emissions[1] & Total_emissions <= input$emissions[2]) #filter based on the range on total emissions
      arrange(desc(!!sym(input$y))) #arrange by selected y (also what they will see on bar chart)
  })
  
  #Render the scatter plot 
  output$scatterplot <- renderPlot({
    ggplot(data = food_filtered(), aes_string(x = input$x, y = input$y, color = "product")) +
      geom_point(size = 3) +
      labs(x = tools::toTitleCase(gsub("_", " ", input$x)),
           y = tools::toTitleCase(gsub("_", " ", input$y))
      ) +
      theme_classic() +
      theme(legend.position = "bottom")
  })
  # Render the bar chart
  output$barchart <- renderPlot({
    ggplot(data = food, aes_string(x = "category", y = input$y, fill = "cateogry")) +
      geom_bar(stat = "identity") +
      xlab("Food Category") +
      ylab(ylab(tools::toTitleCase(gsub("_", " ", input$y)))) +
      ggtitle("Contribution of Different Food Categories to Selected Emission Type") +
      scale_fill_brewer(palette = "Paired") +
      theme_classic() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  # Render the pie chart
  output$piechart <- renderPlot({
    foodprod_sum <- aggregate(food$input$y, by = list(foodtop10$category), sum) #for pie chart
    names(foodprod_sum) <- c("Category", "Total_emissions")
    ggplot(data = foodprod_sum, aes(x = "", y = input$y, fill = category)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Paired") +
      ggtitle("Selected Emission by Food Type") +
      xlab("") +
      ylab("Selected Emission") +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = "Food Category")) +
      theme_classic() +
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank())
  })
  
  #Render data table on the first tab (if checked)
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = food_filtered(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  #Render a second data table for the pie chart tab
  output$piedata <- DT::renderDataTable({
    DT::datatable(data = foodprod_sum,
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
}


#run the dashboard
shinyApp(ui = ui, server = server)