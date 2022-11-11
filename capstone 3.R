library(plyr)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
library(janitor)

data_set = read.csv("E:/Desktop data/IEC/Capstone III/Credit_data.csv",stringsAsFactors = T)


skim_without_charts(credit_data)

#Good & Bad Credit distribution in portfolio
ggplot(data=credit_data)+
  geom_bar(aes(x=Risk),colors(bad= red,good = green))

#group by gender,Job,Risk to find max,min,mean of credit amount
data_set %>% 
  select(Gender,Job,Risk,Credit.amount) %>% group_by(Gender,Job,Risk) %>% na.omit() %>% 
  summarise(maximum_credit=(max(Credit.amount)))

data_set %>% 
  select(Gender,Job,Risk,Credit.amount) %>% group_by(Gender,Job,Risk) %>% na.omit() %>% 
  summarise(minimum_credit=(min(Credit.amount)))

data_set %>% 
  select(Gender,Job,Risk,Credit.amount) %>% group_by(Gender,Job,Risk) %>% na.omit() %>% 
  summarise(mean_credit=(mean(Credit.amount)))

Credit_data = mutate(data_set,Age_Bracket = ifelse(Age <= 30,"Between 18-30",ifelse(Age >30 & Age <= 40 , "Between 31-40", ifelse (Age>40 & Age <=50,"Between 41-50", ifelse(Age>50 & Age<=60,"Between 51-60",ifelse(Age>60,"60 or above",0))))))


Credit_df = Credit_data %>% mutate(Duration_bracket = ifelse(Duration <= 19,"Between 4-19",ifelse(Duration > 19 & Duration <=35,"Between 20-35",ifelse (Duration >35 & Duration <=50,"Between 36-50",ifelse (Duration>50,"51 and above",0)))))

#Converting categorical variables to numeric
Credit_df = Credit_df %>% mutate(Gender_int = ifelse(Gender == "male",0,1)) %>% 
  mutate(Job_int = ifelse(Job == "skilled",0,ifelse(Job=="unskilled and resident",1,ifelse(Job=="highly skilled",2,3)))) %>% 
  mutate(House_int = ifelse(Housing == "own",0,ifelse(Housing == "free",1,2))) %>% 
  mutate(Saving_accounts_int = ifelse(Saving.accounts == 0,0,ifelse(Saving.accounts == "little",1,ifelse(Saving.accounts == "moderate",2,ifelse(Saving.accounts=="rich",3,4))))) %>% 
  mutate(Checking_accounts_int = ifelse(Checking.account == 0,0,ifelse(Checking.account=="little",1,ifelse(Checking.account=="moderate",2,ifelse(Checking.account=="rich",3,4))))) %>% 
  mutate(Purpose_int = ifelse(Purpose == "radio/TV",0,ifelse(Purpose == "education",1,ifelse(Purpose=="furniture/equipment",2,ifelse(Purpose=="car",3,ifelse(Purpose=="business",4,ifelse(Purpose=="domestic appliances",5,ifelse(Purpose=="repairs",6,ifelse(Purpose=="vacation/others",7,8))))))))) %>% 
  mutate(Risk_int = ifelse(Risk == "good",0,1))
view(Credit_df)
write.csv(Credit_df,"C:\\Users\\arsal\\Desktop\\People.csv", row.names = FALSE)
rm(Credit_data)
rm(data_set)

Numeric_data = Credit_df %>%  select(Age,Credit.amount,Duration,Gender_int,Job_int,House_int,Saving_accounts_int,Checking_accounts_int,Purpose_int,Risk_int)
mydata.cor = cor(Numeric_data)

library(Hmisc)
mydata.rcorr = rcorr(as.matrix(Numeric_data))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
corrplot(mydata.cor)

ggplot(data = Credit_df)  + 
  geom_bar(aes(x = Risk, y = Credit.amount,fill = Risk),stat = "Sum") + 
  labs(title = "Credit Portfolio Distribution",
       x = "Category", y = "Total Credit in Euros")

chisq.test(table(Credit_df$Purpose))


ggplot(data=Credit_df)+
  geom_bar(aes(x=Age_Bracket,fill = Age_Bracket)) +
  labs(title = "Distribution of Credits over the different Age groups", x="Groups",y="Number of Credits")
  
ggplot(data = Credit_df)+
  geom_histogram(aes(x=Age_Bracket,y=Credit.amount,fill=Age_Bracket),stat = "sum")

install.packages("corrplot")
library(corrplot)
corrplot(Credit_df,x)

write.csv(mydata.cor,"C:\\Users\\arsal\\Desktop\\mydatacor.csv", row.names = FALSE)
str(data_set)
