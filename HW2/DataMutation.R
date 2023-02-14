library(dplyr)
library(ggplot2)
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

