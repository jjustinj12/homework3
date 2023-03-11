if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


# Subset the data for the relevant years (1970 to 1985) and the "tax_dollar" measure
Q1 <- final.data %>%
  filter(Year >= 1970 & Year <= 1985)%>%
  group_by(Year) 
  




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





#2 
#Plot on a single graph the average tax (in 2012 dollars) 
#on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
final.data1<-final.data%>%
  filter(Year<=2018)
avg_tax<- final.data1%>%
  group_by(Year)%>%
  summarize(AvgTax=mean(tax_dollar, na.rm=TRUE))
avg_costpack<-final.data1 %>%
  group_by(Year)%>%
  summarize(AvgCostPerPack=mean(cost_per_pack, na.rm=TRUE))
avg_costpack
merged_data <- merge(avg_tax, avg_costpack, by = "Year")
merged_data
figure2<-ggplot(data = merged_data, aes(x = Year)) +
  geom_line(aes(y = AvgTax, color = "Average Tax")) +
  geom_line(aes(y = AvgCostPerPack, color = "Average Price per Pack")) +
  labs(title = "Average Tax and Price per Pack from 1980 to 2000",
       x = "Year",
       y = "Average Tax/Price",
       color = "Legend") +
  scale_color_manual(values = c("blue", "green"))

#3 
top_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=cost_per_pack[Year == 2018]-cost_per_pack[Year == 1970])%>%
  arrange(desc(PriceIncrease)) %>%
  head(5)
table_3<-knitr::kable(top_states)
Q3 <- final.data %>%
  filter(state %in% top_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
figure3<-ggplot(data=Q3, aes(x = Year, y = avg_packs_sold)) + 
  geom_line()+
  labs(title = "Average number of packs for the 5 states with the HIGHEST increases in cigarette prices",
       x = "Year",
       y = "Average number of packs sold per capita ")+
  ylim(0,200)
figure3

#4
bottom_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=cost_per_pack[Year == 2018]-cost_per_pack[Year == 1970])%>%
  arrange((PriceIncrease)) %>%
  head(5)
table_4<-knitr::kable(bottom_states)
Q4 <- final.data %>%
  filter(state %in% bottom_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
figure4<-ggplot(data=Q4, aes(x = Year, y = avg_packs_sold)) + 
  geom_line()+
  labs(title = "Average number of packs for the 5 states with the Lowest increases in cigarette prices",
       x = "Year",
       y = "Average number of packs sold per capita ")+
  ylim(0,200)
figure4


#5
#For the states with the highest increase in cigarette prices they had a much higher decrease in the number of packs sold per captia where the highest number of packs sold per caption was approximately close to 140 in 1970 and decrease to a little less than 25 pack per captia. In compairson for the states with the smallest/lowest change in cigarret prices the change in number of packs sold was less pronouced. Specifically there was actually a small increase between 1970 and 1975 where the highest number of packs per captia was 150 in 1975 and decrease to a little more than 50 by 2018.  
