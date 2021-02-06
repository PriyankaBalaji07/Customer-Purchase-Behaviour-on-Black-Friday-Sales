# Install and load all the packages
install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)
install.packages("plotly")
install.packages("data.table")
library(plotly)
library(dplyr)
library(scales)
library(data.table)

# Read the blackfriday dataset by choosing csv file
blackfriday <- read.csv(file.choose(),header = T)

# Structure the data
str(blackfriday)

# Checking missing value
sapply(blackfriday , function(x) sum(is.na(x)))

### Hypothesis Testing 1: Gender Difference
#Define the purchase data of women
female_purchase <- blackfriday %>% 
  filter(Gender == "F") %>% 
  select(Purchase)
#Define the purchase data of men
male_purchase <- blackfriday %>% 
  filter(Gender == "M") %>% 
  select(Purchase)

# Two Sample t-test
t.test(female_purchase, male_purchase, alternative = "greater")

# Plotting the gender difference
ggplot(aes(x = Gender ,  y = Purchase , fill = Gender),data = blackfriday) + 
  geom_boxplot() + 
  ggtitle("The Purchase Behaviour of Women & Men")

# Sampling method to estimate the population
t.test(blackfriday$Purchase , mu = 967.13)

### Hypothesis Testing 2: Marriage Status Difference
ma_purchase <- blackfriday %>% 
  filter(Marital_Status == 1) %>% 
  select(Purchase)
unma_purchase <- blackfriday %>% 
  filter(Marital_Status == 0) %>% 
  select(Purchase)

# Two Sample t-test
t.test(unma_purchase , ma_purchase)


# Plotting the marital status difference
blackfriday %>% 
  ggplot(aes(x = factor(Marital_Status) , y = Purchase , fill = factor(Marital_Status)))+
  geom_boxplot() +
  ggtitle("The Purchase Behaviour of Unmarried vs. Married")

### Hypothesis Testing 3: Age Difference
# The analysis of variance
age.aov <- aov(Purchase ~ Age , data = blackfriday )

# Summarizing the analysis of variance model
summary(age.aov)

# Pairwise comparisons using t tests
pairwise.t.test(blackfriday$Purchase , as.vector(blackfriday$Age))

# Make a plot to visualize the age difference
blackfriday %>% 
  ggplot(aes(x = Age ,y = Purchase ,  fill = Age)) +
  geom_boxplot()+ 
  ggtitle("The Purchase Behaviour of Different Age Group")

### Hypothesis Testing 4
women <- blackfriday %>% 
  filter(Marital_Status == 1 , Age == "26-35", Gender == "F") %>% 
  select(Purchase)
men <- blackfriday %>% 
  filter(Marital_Status == 1 , Age == "26-35", Gender == "M") %>% 
  select(Purchase)

# Two sample t-test
t.test(women , men , alternative = "greater")

# Plotting the bar chart to visualize two groups
blackfriday %>% group_by(Age, Gender) %>% summarize(purchase_amount = sum(as.numeric(Purchase))) %>% ggplot(mapping = aes(x = factor(Age), y = purchase_amount, fill = Gender)) + geom_col() + theme_bw()

# Define the multiline chart
df_multiline <- blackfriday[c(3,4,8,12)]
df_multiline <- df_multiline %>% group_by(Gender,Marital_Status,Age) %>% 
  summarise(meanPurchase= mean(Purchase))
df_casted<-dcast(df_multiline, Age ~ Gender + Marital_Status, value.var = c("meanPurchase"))
df_casted['place'] <- seq(1,7,1)

# Plotting the multiple line chart to visualize the hypothesis testing
ggplotly(
  ggplot(df_casted,aes(x=place))+
    geom_line(aes(y=M_0,color='Male_Single'))+
    geom_point(aes(y=M_0),color='forestgreen',fill='white',size=3,shape=21,stroke=1)+
    
    geom_line(aes(y=M_1,color='Male_Married'))+
    geom_point(aes(y=M_1),color='red',fill='white',size=3,shape=21,stroke=1)+
    
    geom_line(aes(y=F_0,color='Female_Single'))+
    
    geom_point(aes(y=F_0),color='forestgreen',fill='white',size=3,shape=21,stroke=1)+  
    geom_line(aes(y=F_1,color='Female_Married'))+
    geom_point(aes(y=F_1),color='red',fill='white',size=3,shape=21,stroke=1)+
    scale_color_manual( values = c('Male_Single'='turquoise',
                                   'Male_Married'='royalblue1',
                                   'Female_Single'='hotpink',
                                   'Female_Married'='firebrick3'))+
    labs(x="Ages",y="Mean_Purchase")+
    scale_x_discrete(limits=df_casted$Age)+
    theme_light())
