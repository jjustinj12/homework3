if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#1
Q1 <- final.data %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(Year) %>%
  summarize(prop = length(unique(state[tax_dollar != lag(tax_dollar)]))/ length(unique(state)))


graph_1 <- ggplot(Q1, aes(x = Year, y = prop)) + 
  geom_bar(stat = "identity", fill="blue") + 
  labs(title = "Proportion of States with a Change in Cigarette Tax from 1970 to 1985", x = "Year", y = "Proportion of States with a Change") +
  theme_minimal()
graph_1



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
figure2


#3 
top_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=cost_per_pack[Year == 2018]-cost_per_pack[Year == 1970])%>%
  arrange(desc(change_in_price)) %>%
  head(5)
table1<-knitr::kable(top_states)
Q3 <- final.data %>%
  filter(state %in% top_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
figure3<-ggplot(data=Q3, aes(x = Year, y = avg_packs_sold)) + 
  geom_line()+
  labs(title = "Average number of packs for the 5 states with 
  the HIGHEST increases in cigarette prices",
       x = "Year",
       y = "Average number of packs sold per capita ")+
  ylim(0,200)
figure3

#4
bottom_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=cost_per_pack[Year == 2018]-cost_per_pack[Year == 1970])%>%
  arrange((change_in_price)) %>%
  head(5)
table2<-knitr::kable(bottom_states)
table2
Q4 <- final.data %>%
  filter(state %in% bottom_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
figure4<-ggplot(data=Q4, aes(x = Year, y = avg_packs_sold)) + 
  geom_line()+
  labs(title = "Average number of packs for the 5 states with the 
       Lowest increases in cigarette prices",
       x = "Year",
       y = "Average number of packs sold per capita ")+
  ylim(0,200)
figure4


#5
#For the states with the highest increase in cigarette prices they had a much higher decrease in the number of packs sold per captia where the highest number of packs sold per caption was approximately close to 140 in 1970 and decrease to a little less than 25 pack per captia. In compairson for the states with the smallest/lowest change in cigarret prices the change in number of packs sold was less pronouced. Specifically there was actually a small increase between 1970 and 1975 where the highest number of packs per captia was 150 in 1975 and decrease to a little more than 50 by 2018.  

#6
Q6<-final.data%>%
  filter(Year >=1970 & Year <=1990)
model_1 <- lm(log(sales_per_capita) ~ log(cost_per_pack), data = Q6)
summary(model_1)

#For every 1% increase in cost per pack the sales per captia decrease by .17%. Thus demands decreases as there is an icnrease in costs per pack. 


#7 
if (!requireNamespace("AER", quietly = TRUE)) {
  install.packages("AER")}
library(AER)


model_2<- ivreg(log(sales_per_capita) ~ log(cost_per_pack) | log(tax_dollar), data = Q6)

summary(model_2)

# For every 1% increase in cost per pack the sales per captia decrease by .28%. Thus demands decreases as there is an increase in costs per pack.
# This is different from the first model because we are using total dollars as the instrument variable to
# complete the regression. the original estimate the predictor variable (cost per pack) can be correlated to other
# variables beyond sales per capita.
# Thus by using the IV tax dollar we are having a more accurate estimate because we are reducing endogeneity

#8

first_step1 <- lm(log(cost_per_pack) ~  log(tax_dollar), data=Q6)
reduced_form1 <- lm(log(sales_per_capita)~log(tax_dollar), data=Q6)
cost_hat <- predict(first_step1)
Twostage_equivalence1 <- lm(log(sales_per_capita) ~ cost_hat, data=Q6)
summary(Twostage_equivalence1)


#9
Q9<-final.data%>%
  filter(Year >=1991 & Year <=2015)
model_3 <- lm(log(sales_per_capita) ~ log(cost_per_pack), data = Q9)
summary(model_3)
model_4<- ivreg(log(sales_per_capita) ~ log(cost_per_pack) | log(tax_dollar), data = Q9)
summary(model_4)

first_step2 <- lm(log(cost_per_pack) ~  log(tax_dollar), data=Q9)
reduced_form2 <- lm(log(sales_per_capita)~log(tax_dollar), data=Q9)
cost_hat <- predict(first_step2)
Twostage_equivalence2 <- lm(log(sales_per_capita) ~ cost_hat, data=Q9)
summary(Twostage_equivalence2)


save.image("Hwk3_workspace_1.Rdata")

