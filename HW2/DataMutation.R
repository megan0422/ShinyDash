library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
food <- read_csv("HW2/food.csv")

food <- food %>%
  mutate(category = case_when(
    grepl("Beef|Lamb|Pig|Poultry|Fish", product, ignore.case = TRUE) ~ "Meat/Seafood",
    grepl("Potato|Tomato|Citrus|Banana|Apple|Berries|Onions|Root Vegetables|Brassicas", product, ignore.case = TRUE) ~ "Fruit/Vegetable",
    grepl("Dairy|Milk|Cheese|Eggs", product, ignore.case = TRUE) ~ "Dairy/Eggs",
    TRUE ~ "Grain/Nut/Seed")
    )%>%
  relocate(category, .after = product)

View(food)

ggplot(data = food, aes(x = category, y = Total_emissions)) +
  geom_bar(stat = "identity", fill = 'pink') +
  xlab("Food Type") +
  ylab("Emissions") +
  ggtitle("Contribution of Food Types to Total Emissions")

ggplotly(
  ggplot(data = food, aes_string(x = "Animal_feed", y = "Total_emissions", color = "product")) +
    geom_point(size = 5) +
    theme_classic() +
    theme(legend.position = "bottom")
)
colors <- c('#c584e4', '#82ac64', '#00bbd4', '#fef769')

fig <- plot_ly(food, labels = ~category, values = ~Total_emissions, type = 'pie',
               textposition = 'inside',
               textinfo = 'percent',
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)))
fig <- fig %>% layout(title = 'Total Emissions by Food Category',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
