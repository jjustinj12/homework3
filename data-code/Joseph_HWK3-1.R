
read_rds(final.data,"data/output/TaxBurden_Data.rds")

# Subset the data for the relevant years (1970 to 1985) and the "tax_dollar" measure
tax.data <- cig.data %>%
  filter(Year >= 1970 & Year <= 1985 & measure == "tax_dollar")

# Count the number of states with a change in their cigarette tax for each year
tax.count <- tax.data %>%
  group_by(Year) %>%
  summarise(count = sum(value != lag(value, default = first(value))))

# Calculate the proportion of states with a change in their cigarette tax for each year
tax.prop <- tax.count %>%
  mutate(prop = count / n_distinct(cig.data$state_abb))

# Create a bar graph of the proportion of states with a change in their cigarette tax for each year
ggplot(tax.prop, aes(x = Year, y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks = seq(1970, 1985, by = 1)) +
  ylab("Proportion of states with a change in cigarette tax") +
  ggtitle("Proportion of states with a change in cigarette tax by year")