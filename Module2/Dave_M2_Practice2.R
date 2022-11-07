
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(DescTools)
library(dplyr)
library(tidyverse)
library(psych)
library(ggplot2)

# 11.1 (A)

#Creating the vector values of result of samples
v1<- c(12, 8, 24, 6)
v2<-c(0.20,0.28,0.36,0.16)
#Creating vector of distribution of general


#Result of chi square test
?chisq.test()
test_result=chisq.test(x=random_samples, p=general_population)
test_result
#Getting the the critical value
#df = n-1 for samples
qchisq(alpha,df=3)

t.test(p,population,conf.level=0.90)

# 11.1 (B)

#Significance value
alpha<- 0.05

#Creating the vector values.
random_samples<- c(125, 40, 10, 25)

#Creating vector probabilities
flight_probs<-c(0.708,0.082,0.090,0.12)

#Result of chi square test
test_result<-chisq.test(x=random_samples, p=flight_probs)
test_result

#Findind the critical value
qchisq(0.05,df=3)

# 11.2 (A)

#Creating vector

ethnicity = matrix(c(salesAsIndex$n,salesAsColor$n),nrow = 1,byrow=TRUE) 
rownames(ethnicity) = c(2013,2014)
colnames(ethnicity) = c("Caucasian","Hispanic","African American","Other")
ethnicity
result <- chisq.test(ethnicity)

#Summary results
result$statistic
result$p.value
result$parameter
result$statistic

#Calculation critical value
qchisq(p=.05, df=3, lower.tail=FALSE)

# 11.2 (B)

#Creating Vector
r1 <- c(10791,62491)
r2 <- c(7816,42750)
r3 <- c(932,9525)
r4 <- c(11819,54344)
rows=4

women_in_forces = matrix(c(r1,r2,r3,r4),nrow = rows,byrow=TRUE) 

rownames(women_in_forces) = c("Army","Navy","Marine Corps","Air Force")
colnames(women_in_forces) = c("Officers","Enlisted")
women_in_forces
result <- chisq.test(women_in_forces)
result

#Summary results
result$statistic
result$p.value
result$parameter
result$statistic

#Calculating critical value
a <- qchisq(p=.05, df=3, lower.tail=FALSE)

# 12.1
condiments<- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), 'type' = rep('condiments', 7), stringsAsFactors =F)
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), 'type' =rep('cereals', 7), stringsAsFactors = FALSE)
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'type'= rep('desserts', 8), stringsAsFactors =F)
value_sodium<-rbind(condiments, cereals, desserts)
value_sodium$type<- as.factor(sodium$type)
value_sodium

#probs
p<-c(0.708,0.082,0.090,0.12)

#ANOVA test
s.anova<- aov(value_sodium~food, data=value_sodium)
smmry<-summary(s.anova)
smmry

# k - 1
nume <- a.summary[[1]][1, "Df"]
# N - k
domi <- a.summary[[1]][2, "Df"]
domi

#Finding critical value
qf(p=0.05, domi,nume, lower.tail = F)

# 12-2 (A)
alpha = 0.01
cereal <- data.frame('sales'=c(578,320,264,249,237),'food' = rep('cereal',5),stringsAsFactors = FALSE)
chocolate_candy<- data.frame('sales'=c(311,106,109,125,173),'food'=rep('chocolate_candy',5),stringsAsFactors = FALSE)
coffee <- data.frame('sales'=c(261,185,302,689),'food'=rep('coffee',4),stringsAsFactors = FALSE)

qf(p=.01, df1=2, df2=11, lower.tail=FALSE)
sales <- rbind(cereal,chocolate_candy,coffee)
sales$food <- as.factor(sales$food)

anova <- aov(sales~food,data = sales)
summary(anova)

# 12.2.2 (B)
alpha = 0.05
Eastern_third<- data.frame('expenditure'=c(4946,5953,6202,7243,6113),'state' = rep('Eastern third',5),stringsAsFactors = FALSE)
Middle_third<- data.frame('expenditure'=c(6149,7451,6000,6479),'state'=rep('Middle third',4),stringsAsFactors = FALSE)
Western_third<- data.frame('expenditure'=c(5282,8605,6528,6911),'state'=rep('Western third',4),stringsAsFactors = FALSE)

expenditure <- rbind(Eastern_third,Middle_third,Western_third)
expenditure$state <- as.factor(expenditure$state)

anova1 <- aov(expenditure~state,data = expenditure)
ANOVA_expenditure = anova1
summary(ANOVA_expenditure)

# 12.3

#Creating dataframe
plantA1<- data.frame('growth'=c(9.2,9.4,8.9),'light_type' = rep('Grow_light 1',3),'food'=rep("Plant food A",3),stringsAsFactors = FALSE)
plantA2<- data.frame('growth'=c(8.5,9.2,8.9),'light_type' = rep('Grow_light 2',3),'food'=rep("Plant food A",3),stringsAsFactors = FALSE)

plantB1<- data.frame('growth'=c(7.1,7.2,8.5),'light_type' = rep('Grow_light 1',3),'food'=rep("Plant food B",3),stringsAsFactors = FALSE)
plantB2<- data.frame('growth'=c(5.5,5.8,7.6),'light_type' = rep('Grow_light 2',3),'food'=rep("Plant food B",3),stringsAsFactors = FALSE)

company <- rbind(plantA1,plantA2,plantB1,plantB2)
company$light_type <- as.factor(company$light_type)
company$food <- as.factor(company$food)

#Visualization
interaction.plot(company$light_type , company$food, company$growth , 
                 type = "b" , col = c("red","blue"),xlab='Strengths',trace.label = "Density", 
                 pch = c(2,3) ,ylab = "Yield of Plant", ylim = c(5,10),
                 main='Growth of Plants in lights')

#Applying ANOVA
anova1 <- aov(growth~light_type+food+light_type:food,data=company)
anova.summary <-summary(anova1)
anova.summary

# ON YOUR OWN #
#Importing dataset
df<-read.csv("baseball.csv")
summary(df)
str(df)
psych::describe(df)
# MEAN instead NA
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

# SUMMARIES
summary(df)
psych::describe(df)

win <- df %>% group_by(Team) %>% summarize(wins = sum(W)) %>% as.tibble()
win_by_league <- df %>% 
  group_by(League) %>% summarize(wins = sum(W)) %>%
  as.tibble()
hist(df$W,main="Distribution of Winings",xlab='Team wins',col='cyan4',border='white')
hist(df$RS,main="Distribution of Run Scored",xlab='Run Scored',col='cyan4',border='white')
hist(df$RA,main="Distribution of Run Average",xlab='Run Average',col='cyan4',border='white')

# Extract decade from year 
df$Decade <- df$Year - (df$Year %% 10) 

# Create a wins table by summing the wins by decade 
wins <- df %>% 
  group_by(Decade) %>% summarize(wins = sum(W)) %>%
  as.tibble()

#Creating the vector values.
r1<- c(125, 40, 10, 25)

#Creating vector probabilities
p<-c(0.708,0.082,0.090,0.12)

#Result of chi square test
outcome<-chisq.test(wins)
outcome
qchisq(p=0.05, df=5, lower.tail = F)

crop <- read.csv("crop_data.csv")

sum(is.na(crop))

#Data cleaning and preparing
crop$density <- as.factor(crop$density)
crop$block <- as.factor(crop$block)
crop$fertilizer <- as.factor(crop$fertilizer)

#Applying ANOVA
anova <- aov(yield ~ fertilizer+density+fertilizer:density , data = crop)
anova_summary<-summary(anova)
anova_summary
