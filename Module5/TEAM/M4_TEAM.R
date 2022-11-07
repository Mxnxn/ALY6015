setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
articles = read.csv("./articles.csv")
customers = read.csv("./customers.csv")

psych::describe(customers)
summary(customers)
summary(customers)
unique(customers$club_member_status)
library(dplyr)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

customers[customers$club_member_status == "",] = Mode(customers$club_member_status)
clubMemberStatus = customers %>% filter(club_member_status != "")
clubMemberStatus$club_member_status = as.numeric(as.factor(clubMemberStatus$club_member_status))
clubMemberStatus$fashion_news_frequency = as.numeric(as.factor(clubMemberStatus$fashion_news_frequency))

print(prop.table(table(clubMemberStatus$club_member_status))*100)
barplot(prop.table(table(clubMemberStatus$club_member_status))*100,
        names.arg = c("Active","Pre-Create","Left"),
        xlab="Membership",
        col=ifelse(prop.table(table(clubMemberStatus$club_member_status))*100 > 80,'cyan3','grey'),
        border=ifelse(prop.table(table(clubMemberStatus$club_member_status))*100 <5,'gray','white'),
        main="Customers of H&M",
        ylim=c(0,100))

hist(clubMemberStatus$age,
        xlab="Age",
        main="Age Distribution of H&M Customers",col='cyan3',border='white',
        ylim=c(0,300000))

brackets <- clubMemberStatus %>% mutate(agegroup = case_when(age > 0  & age <= 15 ~ 'Teen',
                                             age > 15  & age <= 40 ~ 'Youngsters (15-40)',
                                             age > 40  & age <= 80 ~ 'Elders (40-80)')) # end function

age_brackets = as.data.frame(prop.table(table(brackets$agegroup)) * 100)
ggplot(age_brackets,aes(x=Var1,y=Freq))+geom_bar(stat='identity') +labs(title='Percentage of Age of H&M Customers')

summary(clubMemberStatus$age)

psych::describe(articles)
str(articles)
# PRODUCT SALES RELATIVE PLOT
salesAsType = articles %>%  count(product_type_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq > 2)
barplot(salesAsType$freq,names.arg=salesAsType$product_type_name,ylim=c(0,15),main='Percentage Type of Products of H&M',cex.names = 0.5,las=2,col=ifelse(salesAsType$freq > 3,'Red','cyan4'),border='white')

salesAsTypeGroup = articles %>%  count(product_group_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq > 1)
barplot(salesAsTypeGroup$freq,names.arg=salesAsTypeGroup$product_group_name,ylim=c(0,50),main='Percentage Group of Products of H&M',cex.names = 0.5,las=2,col='Red',border='white')

salesAsGraphics = articles %>%  count(graphical_appearance_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq > 5)
barplot(salesAsGraphics$freq,names.arg=salesAsGraphics$graphical_appearance_name,ylim=c(0,50),main='Percentage Group of Products of H&M',cex.names = 0.6,las=2,col='Red',border='white')

library('ggplot2')
salesAsIndex = articles %>%  count(index_name) %>% mutate(freq = n / sum(n)*100) 
ggplot(salesAsIndex,aes(y=freq,x=index_name))+geom_bar(stat='identity')+theme(axis.text.x = element_text(angle = 45,hjust=1,size=7))+labs(title='Percentage of Index of H&M Sales')

salesAsSection = articles %>%  count(section_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq>2)
ggplot(salesAsSection,aes(y=freq,x=section_name))+geom_bar(stat='identity')+theme(axis.text.x = element_text(angle = 45,hjust=1,size=7))+labs(title='Percentage of Index of H&M Sales')

salesAsColor = articles %>%  count(perceived_colour_value_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq>2)
ggplot(salesAsColor,aes(y=freq,x=perceived_colour_value_name))+geom_bar(stat='identity')+theme(axis.text.x = element_text(angle = 45,hjust=1,size=7))+labs(title='Percentage of Index of H&M Sales')

salesAsColorName = articles %>%  count(perceived_colour_master_name) %>% mutate(freq = n / sum(n)*100) %>% filter(freq>2)
ggplot(salesAsColorName,aes(y=freq,x=perceived_colour_master_name))+geom_bar(stat='identity')+theme(axis.text.x = element_text(angle = 45,hjust=1,size=7))+labs(title='Percentage of Index of H&M Sales')


# HYPOTHESIS TESTING
#4 
tCust = select(customers,c('club_member_status','age','fashion_news_frequency'))
tCust = tCust %>% filter(age != 'ACTIVE')
tCust = tCust %>% filter(fashion_news_frequency != "")
customers = customers %>% filter(fashion_news_frequency != "")
customers$fashion_news_frequency = ifelse(customers$fashion_news_frequency == 'None',"NONE",customers$fashion_news_frequency)
tCust$fashion_news_frequency = ifelse(tCust$fashion_news_frequency == 'None',"NONE",tCust$fashion_news_frequency)
tCust$fashion_news_frequency = as.numeric(as.factor(tCust$fashion_news_frequency))
tCust$club_member_status = as.numeric(as.factor(tCust$club_member_status))
#unique(tCust$fashion_news_frequency)
#unique(customers$fashion_news_frequency)

#s4.anova<- aov(club_member_status ~ age + fashion_news_frequency + age:fashion_news_frequency, data=tCust)
#summary(s4.anova)


tCust <- tCust %>% mutate(ageGroup = case_when(age >= 50  & age <= 100 ~ 3,
                                             age >= 30  & age <= 50 ~ 2,
                                              age <= 30 ~ 1))
tCust <- tCust %>% mutate(fnf = case_when(fashion_news_frequency == "NONE"~ 1,
                                          fashion_news_frequency == "ACTIVE" ~ 3,
                                          fashion_news_frequency == "Regularly" ~ 2)
                          )

tukey = TukeyHSD(aov(club_member_status ~ ageGroup * fnf , data=tCust),conf.level = 0.95)
par(mar=c(6,8,3,2))
plot(tukey,las=2)
tukey

plot(club_member_status ~ ageGroup * fnf, data=tCust)
lines(club_member_status ~ ageGroup * fnf, tCust, lwd=2, col="green")

unique(tCust$fnf)

#1
v1 = select(salesAsIndex,-c(freq))
v2 =  select(salesAsColor,-c(freq))
colnames(v1) = c('Name','Sales')
colnames(v2) = c('Name','Sales')
mValues = rbind(v1,v2)
mValues$Name<- as.numeric(as.factor(mValues$Name))


s.anova<- aov(Name~Sales, data=mValues)
smmry<-summary(s.anova)
smmry

#2
tSales = matrix(c(salesAsIndex$n,salesAsColor$n),nrow = 1,byrow=TRUE) 
rownames(tSales) = c('Sales')
colnames(tSales) = c(salesAsIndex$index_name,salesAsColor$perceived_colour_value_name)
tSales
###
result <- chisq.test(table(articles$product_type_name,articles$perceived_colour_master_name))
result

#3
tSalesG = matrix(c(salesAsGraphics$n),nrow = 1,byrow=TRUE)
rownames(tSalesG) = c('Sales')
colnames(tSalesG) = c(salesAsGraphics$graphical_appearance_name)
tSalesG
###########
result <- chisq.test(table(articles$product_type_name, articles$graphical_appearance_name))
result

# TEST TRAIN SPLIT AND GLM
library(misclassGLM)

featureGraphics = as.data.frame(articles$graphical_appearance_name)
featureGraphics$colour = as.numeric(as.factor(articles$perceived_colour_master_name))
featureGraphics$Solid = ifelse(featureGraphics$`articles$graphical_appearance_name` =='Solid',1,0)
colnames(featureGraphics) = c("graphical_name","colour",'solid')
featureGraphics$graphical_name = as.numeric(as.factor(featureGraphics$graphical_name))

mIndex =  sample(c(1,0), nrow(featureGraphics), 
                 replace = T, 
                 prob = c(0.7,0.3))
train_x = featureGraphics[mIndex == 1,]
test_x = featureGraphics[mIndex == 0,]
head(train_x)

LR_model <- glm(solid ~ graphical_name + colour,
                data = train_x, 
                family = binomial(link = "logit"))
summary(LR_model)
plot(LR_model)

prob.train_x = predict(LR_model,
                       newdata = train_x,
                       type = "response")
cm_data = as.factor(ifelse
                    (prob.train_x >= 0.5,
                      1,0))

library(caret)
confusionMatrix(cm_data,
                as.factor(ifelse(train_x$solid == 1, 1,0)))

prob.test_x = predict(LR_model, 
                      newdata = test_x, 
                      type = "response")
prob.test_x

cm_data = as.factor(ifelse
                    (prob.test_x >= 0.5, 
                      1, 0))
cm_data
head(cm_data)

confusionMatrix(cm_data, 
                as.factor(ifelse(test_x$solid == 1, 1,0)), 
                )

library(pROC) 
ROC = roc (test_x$solid, prob.test_x)
X =  plot(ROC,
          col = "black", 
          ylab = "Sensitivity = TP rate", 
          xlab = 'specificity = FP rate')

#08  Calculate and interpret the AUC.
AUC = auc(ROC) 
AUC


