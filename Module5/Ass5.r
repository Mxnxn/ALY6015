alpha <- 0.05
median <- 3000

attendance <- c(6210,3150,2700,3012,4875,3540,6127,2581,2642,2573,2792,2800,2500,3700,6030,5437,2758,3490,2851,2720)

difference <- attendance - median

pos <- length(difference[difference>0])

neg <- length(difference[difference<0])
result <- binom.test(x=c(pos,neg),alternative = "two.sided")
result 

result$p.value
ifelse(result$p.value>alpha ,"fail to reject the null","reject the null")

################################################
##13. 

alpha <- 0.05
n <- 40
pos <- 15
neg <- n - pos

result2 <- binom.test(x=c(pos,neg),alternative = )
result2 

result$p.value
ifelse(result$p.value>alpha ,"fail to reject the null","reject the null")


###############################################
## 13.3 (4)
alpha = 0.05

males <- c(8,12,6,14,22,27,32,24,26,19,15,13)
female <- c(7,5,2,3,21,26,30,9,4,17,23,12,11,16)

result3 <- wilcox.test(x = males,y= female,alternative = "two.sided",correct = FALSE)
result3
result3$p.value
ifelse(result3$p.value>alpha ,"fail to reject the null","reject the null")

############################################
##13.3(8)
alpha <- 0.05
Nl <- c(89,96,88,101,90,91,92,96,108,100,95)
Al <- c(108,86,91,97,100,102,95,104,95,89,88,101)

result4 <- wilcox.test(x = Nl,y= Al,alternative = "two.sided",correct = FALSE)
result4
result4$p.value
ifelse(result4$p.value>alpha ,"fail to reject the null","reject the null")

############################################
###13.5

rm(list=ls())
# performing kruskal wallis test on diffrent regions for the maths scores> 
western<-data.frame(score = c(527,406,474,381,411),group=rep("Western",5))
europe<-data.frame(score = c(520,510,513,548,596),group=rep("Europe",5))
E.asia<-data.frame(score = c(523,547,547,391,549),group=rep("Eastern Asia",5))

data <- rbind(western,europe,E.asia)                   

result6 <- kruskal.test(score~group,data=data)
result6

############################################
##13.6
alpha <- 0.05
city <- c(1,2,3,4,5,6)
subway <- c(845,494,425,313,108,41)
rail <- c(39,291,142,103,33,38)

data <- data.frame(city=city,subway = subway,rail = rail) 

result7 <- cor.test(data$subway,data$rail,method="spearman")
result7

result7$p.value
result7$estimate

#############################################
###14

x<-c("1",
     "2",
     "3",
     "4")
prizes <- sample(x, 40, replace = TRUE, prob = c(0.25, 0.25, 0.25,0.25))
mean(table(prizes))

x<-c("b",
     "i",
     "g")

ticket<-sample(x, 30, replace = TRUE, prob = c(0.6, 0.3, 0.1))

ticket

mean(table(ticket))