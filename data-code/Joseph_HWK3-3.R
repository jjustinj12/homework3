if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

if (!requireNamespace("AER", quietly = TRUE)) {
  install.packages("AER")}
library(AER)
library(fixest)
library("modelsummary")
library("kableExtra")
library(knitr)

#1


final.data<-final.data %>%
  group_by(state) %>% 
  arrange(state, Year)%>%
  mutate (tax_change= tax_state-lag(tax_state),
          tax_change_d=ifelse(tax_change==0,0,1),
          price_cpi_2012=cost_per_pack*(229.5939/index),
          total_tax_cpi_2012=tax_dollar*(229.5939/index),
          ln_sales=log(sales_per_capita),
          ln_price_2012=log(price_cpi_2012))

figure1<-final.data %>%
  group_by(Year)%>%
  filter(Year<1986, Year>1978)%>%
  summarise(mean_change=mean(tax_change_d))%>%
  ggplot(aes(x=as.factor(Year),y=mean_change))+ geom_bar(stat="identity") +
  labs(
    x="Year", 
    y="Proportion of States", 
    title = "Proportion of States with a change in their Cigarette Tax") +theme_update() +ylim(0,1)

figure1
#2 
#Plot on a single graph the average tax (in 2012 dollars) 
#on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
final.data1<-final.data%>%
  filter(Year<=2018)
avg_tax<- final.data1%>%
  group_by(Year)%>%
  summarize(AvgTax=mean(total_tax_cpi_2012, na.rm=TRUE))
avg_costpack<-final.data1 %>%
  group_by(Year)%>%
  summarize(AvgCostPerPack=mean(price_cpi_2012, na.rm=TRUE))
avg_costpack
merged_data <- merge(avg_tax, avg_costpack, by = "Year")
merged_data
figure2<-ggplot(data = merged_data, aes(x = Year)) +
  geom_line(aes(y = AvgTax, color = "Mean Tax")) +
  annotate("text", x = max(merged_data$Year), y = merged_data$AvgTax[nrow(merged_data)], 
           label = "Mean Tax", color = "blue", hjust = 1.5, vjust = -8) +
  geom_line(aes(y = AvgCostPerPack, color = "Mean Price")) +
  annotate("text", x = max(merged_data$Year), y = merged_data$AvgCostPerPack[nrow(merged_data)], 
           label = "Mean Price", color = "darkgreen", hjust =1, vjust = 25) +
  labs(title = "Average Tax and Price per Pack from 1980 to 2000",
       x = "Year",
       y = "Average Tax/Price",
       color = "") +
  scale_color_manual(values = c("blue", "darkgreen"), labels = c("Mean Tax", "Mean Price"))
figure2

#3 
top_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=price_cpi_2012[Year == 2018]-price_cpi_2012[Year == 1970])%>%
  arrange(desc(change_in_price)) %>%
  head(5)
table1<-knitr::kable(top_states)
table1

figure3<-final.data%>%
  filter(state %in% top_states$state) %>%
  group_by(state)%>%
  ggplot(aes(x=Year, y=sales_per_capita, group=state, color=state))+geom_line()+
  labs(title = "Average Number of Packs Sold between 1970-2018 for the states with the smallest increases in cigarette prices",
       x = "Year",
       y = "Number of Packs Sold Per Capita",
       color = "Legend") + scale_color_discrete(name = "state")
figure3



#4
bottom_states<-final.data%>%
  group_by(state)%>%
  summarize(change_in_price=price_cpi_2012[Year == 2018]-price_cpi_2012[Year == 1970])%>%
  arrange((change_in_price)) %>%
  head(5)
table2<-knitr::kable(bottom_states)
table2

figure4<-final.data%>%
  filter(state %in% bottom_states$state) %>%
  group_by(state)%>%
  ggplot(aes(x=Year, y=sales_per_capita, group=state, color=state))+geom_line()+
  labs(title = "Average Number of Packs Sold between 1970-2018 for the states with the smallest increases in cigarette prices",
       x = "Year",
       y = "Number of Packs Sold Per Capita",
       color = "Legend") + scale_color_discrete(name = "state")
figure4

#


#5

Q3_5 <- final.data %>%
  filter(state %in% top_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
Q4_5 <- final.data %>%
  filter(state %in% bottom_states$state) %>%
  group_by(Year)%>%
  summarize(avg_packs_sold=mean(sales_per_capita, na.rm=TRUE))
combined_df <- bind_rows(
  Q3 %>% mutate(group = "States with the Highest 
increases in cigarette prices"),
  Q4 %>% mutate(group = "States with the Lowest 
increases in cigarette prices")
)

figure5<-ggplot(combined_df, aes(x = Year, y = avg_packs_sold, color = group)) +
  geom_line() +
  labs(title = "Average number of packs sold per capita in Top and Bottom States",
       x = "Year",
       y = "Average number of packs sold per capita") +
  theme_bw() + ylim(0,200)
figure5

#For the states with the highest increase in cigarette prices they had a much higher decrease in the number of packs sold per captia where the highest number of packs sold per caption was approximately close to 140 in 1970 and decrease to a little less than 25 pack per captia. In compairson for the states with the smallest/lowest change in cigarret prices the change in number of packs sold was less pronouced. Specifically there was actually a small increase between 1970 and 1975 where the highest number of packs per captia was 150 in 1975 and decrease to a little more than 50 by 2018.  

#6
Q6<-final.data%>%
  filter(Year >=1970 & Year <=1990)
model_1 <- feols(ln_sales ~ ln_price_2012, data = Q6)
summary(model_1)




# For every 1% increase in cost per pack the sales per captia decrease by .809%. Thus demands decreases as there is an icnrease in costs per pack.


#7 
if (!requireNamespace("AER", quietly = TRUE)) {
  install.packages("AER")}
library(AER)


model_2<- feols(ln_sales ~ 1 | ln_price_2012 ~ (total_tax_cpi_2012), data = Q6)
summary(model_2)

# For every 1% increase in cost per pack the sales per captia decrease by .736%. Thus demands decreases as there is an increase in costs per pack.
# This is different from the first model because we are using total dollars as the instrument variable to
# complete the regression. the original estimate the predictor variable (cost per pack) can be correlated to other
# variables beyond sales per capita.
# Thus by using the IV tax dollar we are having a more accurate estimate because we are reducing endogeneity

#8

first_step1 <- feols(ln_price_2012 ~  (total_tax_cpi_2012), data=Q6)
reduced_form1 <- feols(ln_sales~(total_tax_cpi_2012), data=Q6)

#9
Q9<-final.data%>%
  filter(Year >=1991 & Year <=2015)

model_3 <- feols(ln_sales ~ ln_price_2012, data = Q9)
summary(model_3)
model_4<- feols(ln_sales ~ 1 | ln_price_2012 ~ (total_tax_cpi_2012), data = Q9)
summary(model_4)
first_step2 <- feols(ln_price_2012 ~  (total_tax_cpi_2012), data=Q9)
reduced_form2 <- feols(ln_sales~(total_tax_cpi_2012), data=Q9)

justin<-modelsummary(list("OLS"=model_3, "IV"=model_4), title="Point Estimates for 1991-2015", 
                     coef_map=c('ln_price_2012'="Log Price",
                                'fit_ln_price_2012'="Log Price"), 
                     gof_map=c("N"="nobs", "r.squared"),
                     output = "kableExtra")

save.image("Hwk3_workspace_3.Rdata")

