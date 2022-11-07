#install.packages('tidyverse')
#install.packages('ISLR')
#install.packages('psych')
#install.packages('ggridges')
#install.packages("simex")
#install.packages("InformationValue")
#install.packages("caret")
#install.packages('Hmisc')
#### INSTALL IF NOT #####
library(tidyverse)
library(ISLR)
library(ggridges)
library(simex)
library(InformationValue)
library(caret)
library(Hmisc)
data(College)
psych::describe(College)

hist(College$F.Undergrad, 
     col = 'red',border='white', main = paste("Number of applications received"))
df = subset(College)
library(corrplot)
corrplot(cor(df[2:18]))

nonPCollege = College %>% filter(College$Private == 'No')
PCollege = College %>% filter(College$Private == 'Yes')
psych::describe(nonPCollege)
psych::describe(PCollege)
hist(nonPCollege$Grad.Rate,main="Graduation Rate of Non-Private",xlab='Graduation Rate')
hist(PCollege$Grad.Rate,main="Graduation Rate of Private",xlab='Graduation Rate',col='red',border='white')

lmdl = lm(df$Enroll~df$Accept)
plot(df$Enroll~df$Accept ,main=" Enroll ~ Accept Students",xlab='Accepted Students',ylab='Enrolled Student')
abline(a=lmdl$coefficients[1],b=lmdl$coefficients[2])
summary(lmdl)

#02 Spliting the Data

set.seed(120)
?set.seed
mIndex =  sample(2, nrow(College), 
                   replace = T, 
                   prob = c(0.7,0.3))
train_x = College[mIndex == 1,]
test_x = College[mIndex == 2,]
head(train_x)

#03

train_y2 <- glm(Private ~ Apps + Accept + F.Undergrad + Outstate + PhD,
            data = train_x, 
            family = binomial(link = "logit"))
summary(train_y2)

#04
#install.packages("e1071")
library(caret)

# Confusion Matrix

pred_y = predict(train_y2,
                        newdata = train_x,
                        type = "response")
pred_y_classified = as.factor(ifelse
                            (pred_y >= 0.5,
                              "Yes", "No"))

confusionMatrix(pred_y_classified, 
                train_x$Private,
                positive = "Yes")

#install.packages("misclassGLM")
library(misclassGLM)

#05  Report and interpret metrics for Accuracy, Precision, Recall, and Specificity.
TP = 382 # True +ve
TN = 141 # True -ve
FN = 6  # False -ve
FP = 12  # False +ve

# Predicted Accuracy
Accuracy = (TN + TP)/(TN+FP+FN+TP)
Accuracy

# Actual Accuracy 
212/(212+565)

Precision = TP/(FP+TP)
Precision

Recall = TP/(TP+FN)
Recall

Specificity = TN/(TN+FP)
Specificity


#06 Create a confusion matrix and report the results of your model for the test set.
pred_y = predict(GLM2, 
                       newdata = test_x, 
                       type = "response")
pred_y

PredictClassMin = as.factor(ifelse
                            (pred_y >= 0.5, 
                              "Yes", "No"))
PredictClassMin
head(PredictClassMin)

confusionMatrix(PredictClassMin, 
                test_x$Private, 
                positive = "Yes")

#07  Plot and interpret the ROC curve.
library(pROC) 
ROC = roc (test_x$Private, pred_y)
X =  plot(ROC,
          col = "black", 
          ylab = "Sensitivity = TP rate", 
          xlab = 'specificity = FP rate')

#08  Calculate and interpret the AUC.
AUC = auc(ROC) 
cat("ROC area under the curve = ", AUC)
