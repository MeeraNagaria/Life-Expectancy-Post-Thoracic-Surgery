library(readxl)
library(neuralnet)
library(psych)
library(dplyr)
library(car)
library(MASS)
library(gvlma)
library(caret)
library(gains)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)
library(GGally)
library(gmodels)
library(class)

setwd("C:/Users/meera/OneDrive - Northeastern University/SEM 2/Data mining/Case study")
input<- read_excel("Input data.xlsx",sheet="Sheet1")
str(input)
head(input)
#data pre-processing
input <- input %>% mutate(Diagnosis = case_when(
  Diagnosis == 'DGN1' ~ 1,
  Diagnosis == 'DGN2' ~ 2,
  Diagnosis == 'DGN3' ~ 3,
  Diagnosis == 'DGN4' ~ 4,
  Diagnosis == 'DGN5' ~ 5,
  Diagnosis == 'DGN6' ~ 6,
  Diagnosis == 'DGN8' ~ 8))
input

input <- input %>% mutate(PRE6 = case_when(
  PRE6 == 'PRZ0' ~ 0,
  PRE6 == 'PRZ1' ~ 1,
  PRE6 == 'PRZ2' ~ 2))
input

input <- input %>% mutate(PRE7 = case_when(
  PRE7 == 'F' ~ 0,
  PRE7 == 'T' ~ 1))
input

input <- input %>% mutate(PRE8 = case_when(
  PRE8 == 'F' ~ 0,
  PRE8 == 'T' ~ 1))
input

input <- input %>% mutate(PRE9 = case_when(
  PRE9 == 'F' ~ 0,
  PRE9 == 'T' ~ 1))
input

input <- input %>% mutate(PRE10 = case_when(
  PRE10 == 'F' ~ 0,
  PRE10 == 'T' ~ 1))
input

input <- input %>% mutate(PRE11 = case_when(
  PRE11 == 'F' ~ 0,
  PRE11 == 'T' ~ 1))
input

input <- input %>% mutate(PRE14 = case_when(
  PRE14 == 'OC11' ~ 1,
  PRE14 == 'OC12' ~ 2,
  PRE14 == 'OC13' ~ 3,
  PRE14 == 'OC14' ~ 4))
input

input <- input %>% mutate(PRE17 = case_when(
  PRE17 == 'F' ~ 0,
  PRE17 == 'T' ~ 1))
input

input <- input %>% mutate(PRE19 = case_when(
  PRE19 == 'F' ~ 0,
  PRE19 == 'T' ~ 1))
input

input <- input %>% mutate(PRE25 = case_when(
  PRE25 == 'F' ~ 0,
  PRE25 == 'T' ~ 1))
input

input <- input %>% mutate(PRE30 = case_when(
  PRE30 == 'F' ~ 0,
  PRE30 == 'T' ~ 1))
input

input <- input %>% mutate(PRE32 = case_when(
  PRE32 == 'F' ~ 0,
  PRE32 == 'T' ~ 1))
input

input <- input %>% mutate(Risk1Y = case_when(
  Risk1Y == 'F' ~ 0,
  Risk1Y == 'T' ~ 1))
input

#setwd("C:/Users/meera/OneDrive - Northeastern University/SEM 2/Data mining/Case study")
#input<- read_excel("Input data.xlsx",sheet="Sheet1")
head(input)
str(input)
#input$Risk1Y<-as.factor(input$Risk1Y)

#Part 1- Principal Component Analysis and Factor Analysis

#Component Analysis
plot<-fa.parallel(input,fa='pc',n.iter=100,show.legend=TRUE,main="Scree plot with parallel analysis")

input_analysis<-principal(input,nfactors=3,rotate = "promax",score=TRUE)
input_analysis

head(input_analysis$scores)

graph<-factor.plot(input_analysis,labels =rownames(input_analysis$loadings) )

#Interpretation : 3- Principal components, The 1st component contributes 12%, 2nd component- 9% and third component contributes again 9%.
# The 1st component has basically correlates most strongly with 2 variables: PR6 (Performance status - Zubrod scale (PRZ2,PRZ1,PRZ0)
#and PR10 (Cough before surgery (T,F)
#The 2nd component has strong correlation with variables: PR9(Dyspnoe b4 surgery), PR7 (Pain before surgery) and PR8(Haemoptysis b4 surgery)

#Factor Analysis
fa.parallel(input,n.obs=470,fa="fa",n.iter=100,show.legend=TRUE, main="Scree plot with parallel analysis")

#rotate the factors
input_varimax<-fa(input, nfactors=5,rotate = "varimax")

#compute scores
head(factor.scores(input,input_varimax))

#orthogonal solution
factor.plot(input_varimax)

#oblique soution
input_promax<-fa(input, nfactors=5,rotate = "Promax")
fa.diagram(input_promax, simple=FALSE)

#Interpretation : In total 5 factors are above the eigen value 1. Factor 1 is strongly related with the variabes : PRE6, PRE10, PRE11, Age-(Factor 4) are strongled related with the reponse class Risk1Y. 
#PRE5, PRE7, PRE 8, PRE 9 are related to ech other - (Factor 3).Factor 2 has only 1 variable relating to it- diagnosis. Also Age is inversly related with PRE4. 

# Part 2:Vizualizations 
# (a).Scatterplot matrix

ggplot(input, aes(x = PRE4, y = PRE5, colour = Risk1Y)) + 
  geom_point() + 
  xlab("Forced Vital Capacity") + 
  ylab("Performance Status")

ggplot(input, aes(x = PRE4, y = PRE5, colour = Risk1Y)) + 
  geom_point() + 
  xlab("Forced Vital Capacity") + 
  ylab("Performance Status")

hist(input$PRE4,xlab="Performance Status", main="PRE6: Performance status - Zubrod scale (PRZ2,PRZ1,PRZ0)",col="red", border="blue")

scatter

par(mfcol = c(3,3))
boxplot(input$PRE4, xlab = "PRE4", ylab = "area")
boxplot(input$PRE5, xlab = "PRE5", ylab = "area")
boxplot(input$PRE6, xlab = "PRE6", ylab = "area")
boxplot(input$PRE7, xlab = "PRE7", ylab = "area")
boxplot(input$PRE8, xlab = "PRE8", ylab = "area")
boxplot(input$PRE9, xlab = "PRE9", ylab = "area")
boxplot(input$PRE10, xlab = "PRE10", ylab = "area")
boxplot(input$PRE11, xlab = "PRE11", ylab = "area")
boxplot(input$Age, xlab = "PRE14", ylab = "area")

p <- ggplot(input, aes(x = Age, y = PRE8))+
  geom_col(aes(fill = Risk1Y), width = 0.7)
p

plot(input[,1:8])
ggpairs(input[,c(1,2,3,4,5,6,7,8,9,10,16,17)])

# Part 3: Diagnostics Anaytics
#Multi linear regression
M_lr <- lm(Risk1Y ~., data=input)
summary(M_lr)
colnames(input)

input$Age 

#reduced data
M_lr_reduced <- lm(Risk1Y~ Diagnosis + PRE9 + PRE14 + PRE17 , data=input)
summary(M_lr_reduced)

M_lr_reduced1 <- lm(Risk1Y~ PRE9 + PRE14 + PRE17 , data=input)
summary(M_lr_reduced1)

#MLR- with interactions
M_lr1 <- lm(Risk1Y ~ PRE4,I(PRE5)^2, data=input)
summary(M_lr1)

M11 <- lm(Risk1Y ~ Diagnosis + PRE4 + PRE5 + PRE6 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11+ PRE14+ Age+ PRE14 : PRE4 +PRE14 : PRE5+ PRE14:PRE6+ PRE14 : PRE11 , data=input)
summary(M11)

#Multi polyomial regression
M_poly <- lm(Risk1Y ~ (Diagnosis + PRE4 + PRE5 + PRE6 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11+ PRE14+Age)^2, data=input)
summary(M_poly)

cor(input)

#Diagnostics- typical
confint(M_lr)
confint(M11)
confint(M_poly)
#PRE9 with Diagnosis, PRE7, PRE11, PRE10 has the strongest relation.
#PRE9 with Diagnosis is [0.1775625291  0.7929307047]
#PRE9 with PRE7 is [ -0.2589662750  0.8698563749]
#PRE9 with PRE11 is [0.2607348700, 1.4911786127]
#PRE9 with PRE11 is [0.3142330377  1.5676553553]

#enhanced approach
par(mfrow = c(1,3))
qqPlot(M_lr, labels = row.names(Risk1Y), id.method = "identify", simulate = TRUE, main = "Q-Q Plot (MLR)")
qqPlot(M11, labels = row.names(Risk1Y), id.method = "identify", simulate = TRUE, main = "Q-Q Plot (MLRI)")
qqPlot(M_poly, labels = row.names(Risk1Y), id.method = "identify", simulate = TRUE, main = "Q-Q Plot (MLR Poly)")

#We can observe that all the plots above are similar and the points are around the line and within the confidence envelope. Therefore we can say that normality condition is met for all of them.

#outliers
outlierTest(M_lr)
outlierTest(M11)
outlierTest(M_poly)
#No outliers as such observed 

#Origina Models:
anova(M_lr, M11, M_poly)
AIC(M_lr, M11, M_poly)

#Variable Importance using Regression Algo

str(input)
#input<-input[,c(-11,-12,-13,-14,-15)]

Regression_t1 <- rpart(Risk1Y ~ ., data= input,method= "anova", control = rpart.control(maxdepth = 3))

printcp(Regression_t1)

summary(Regression_t1)
#Using Regression Algorithm, we found the Variable Importance for the best performance of the algorithm.
#Top 7 most Imp vars are: PRE14, PRE5, PRE4, PRE30, Diagnosis, Age and PRE6

#Part 4: Implementation
# Algorithm 1: Neural Nets
input_nn<-input[]
#input_nn<-input[,c(1,2,3,4,10,14,16,17)]
str(input_nn)

#Normalize the data/Scale the data
max = apply(input_nn , 2 , max)
min = apply(input_nn, 2 , min)
scaled_nn = as.data.frame(scale(input_nn, center = min, scale = max - min))

str(scaled_nn)

#Dividing the dataset into training and validation
training_data=sample(row.names(scaled_nn), dim(scaled_nn)[1]*0.75)
validation_data=setdiff(row.names(scaled_nn), training_data)

train_df =as.data.frame (scaled_nn[training_data, ])
valid_df = as.data.frame( scaled_nn[validation_data, ])

#Applying Nueral Nets Algo
#nn_impvar <- neuralnet(Risk1Y ~ Diagnosis+PRE4+PRE5+PRE6+PRE7+PRE8+PRE9+PRE10+PRE11+PRE14+Age, data = train_df, hidden = 1, threshold = 0.01, linear.output = T, algorithm = "rprop+",learningrate = 0.1, stepmax = 1e7)
nn_impvar <- neuralnet(Risk1Y ~ ., data = train_df, hidden = 1, threshold = 0.01, linear.output = T, algorithm = "rprop+",learningrate = 0.1, stepmax = 1e7)

nn_impvar
plot(nn_impvar)
nn_impvar$result.matrix
nn.results <- compute(nn_impvar, valid_df[,-17])
results <- data.frame(actual = valid_df$Risk1Y, prediction = nn.results$net.result)
nn.results <- (nn.results$net.result * (max(valid_df$Risk1Y) - min(valid_df$Risk1Y))) + min(valid_df$Risk1Y)
RMSE.nn_valid = (sum((valid_df$Risk1Y - nn.results)^2) / nrow(valid_df)) ^ 0.5
print(1-RMSE.nn_valid)

plot(valid_df$Risk1Y, nn.results, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Algorithm 2: Support Vector Machine -83%
set.seed(1234)
#input_svm<-input[,c(1,2,3,4,5,6,7,8,9,10,16,17)]
input_svm<-input[]

str(input_svm)

training_data=sample(row.names(input_svm), dim(input_svm)[1]*0.75)
validation_data=setdiff(row.names(input_svm), training_data)

train_df =as.data.frame (input_svm[training_data, ])
valid_df = as.data.frame( input_svm[validation_data, ])

svm_model <- svm(Risk1Y ~ ., data=train_df, cost=100, gamma =1,trControl = trainControl(method = "cv"), method = "svmPoly")
summary(svm_model)

pred <- predict(svm_model,valid_df[,-17])

table(pred,valid_df$Risk1Y)

data.frame(actual = valid_df$Risk1Y[1:5], predicted = pred[1:5])
#RMSEsvm <- rmse(actual,Predicted)
fitted.results.svm <- ifelse(pred > 0.5,1,0)

fitted.results.svm<-as.factor(fitted.results.svm)

cm<-confusionMatrix(data=fitted.results.svm,reference=as.factor(valid_df$Risk1Y))
Accuracy_svm<-round(cm$overall[1],2)
Accuracy_svm

#Algorithm 3: Logistic Regression- Accuracy 85%
set.seed(1234)
pred_logit <- glm(Risk1Y ~ ., data = train_df, family =binomial)
summary(pred_logit)

coef(pred_logit)

exp(coef(pred_logit))

logit.reg.pred<-predict(pred_logit, newdata = valid_df[,-17], type = "response")

data.frame(actual = valid_df$Risk1Y[1:5], predicted = logit.reg.pred[1:5])

fitted.results.cat <- ifelse(logit.reg.pred > 0.5,1,0)

require(caret)   
fitted.results.cat<-as.factor(fitted.results.cat)
cm<-confusionMatrix(data=fitted.results.cat,reference=as.factor(valid_df$Risk1Y))

Accuracy_logistic<-round(cm$overall[1],2)
Accuracy_logistic

#Accuracy - 85%
