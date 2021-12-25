rm(list=ls())

library(dplyr)
library(tidyverse)
library(caret)

## importing the  dataset 
bank <- read.csv("C:/Users/UmmeSalma/Documents/R/dana 4820/group project/banksample.csv")

str(bank)
View(bank)
attach(bank)

numeric_vars <- select_if(bank, is.numeric)
M<-cor(numeric_vars)
M
cor(numeric_vars)

library(corrplot)
corrplot(M, method="circle")

# No significant correlation of any variable with 'y' except duration with value 0.38
# Purposeful selection: Choose any 3 variables of choice which are of importance to the Response.
# perform stepwise regression to determine the selection procedure


colnames(numeric_vars) 
str(bank)

cat_vars <- bank[c("job", "marital", "education", "default", "housing", "loan", "contact", "campaign","previous","poutcome","age_group")]
names(cat_vars)

num_vars = bank[,!(names(bank) %in% names(cat_vars))]
names(num_vars)

cat_vars_y <- bank[c("job", "marital", "education", "default", "housing", "loan", "contact", "campaign","previous","poutcome","age_group", "y")]
names(cat_vars_y)
str(cat_vars_y)

bank <- bank[-c(1)]
str(bank)
bank2 <- bank[-c(4, 9)]
names(bank2)

## Chisq
Chisq1 <- lapply(cat_vars_y[,-12], function(x) chisq.test(cat_vars_y[,12], x))
Chisq1

## Checking interactions
lmfit <- lm(y ~ (.)^2, data=bank)
lmfit2 <- lm(y ~(.)^2, data=bank2)
anovafit <- anova(lmfit)
anovafit
anova(lmfit2)
interactions <- anovafit[which(anovafit$`Pr(>F)` < .05),]
interactions

interactionstronger <- anovafit[which(anovafit$`Pr(>F)` < .001),]
interactionstronger
anova(lmfit2)[which(anova(lmfit2)$'Pr(>F)' <.001),]

# interaction plots for p-val<<.0001
interaction.plot(bank$job, bank$month, bank$y)
interaction.plot(bank$job, bank$duration, bank$y)
interaction.plot(bank$job, bank$poutcome, bank$y)
interaction.plot(bank$marital, bank$duration, bank$y)
interaction.plot(bank$housing, bank$duration, bank$y)
interaction.plot(bank$housing, bank$pdays, bank$y)
interaction.plot(bank$contact, bank$duration, bank$y)
interaction.plot(bank$duration, bank$pdays, bank$y)
interaction.plot(bank$poutcome, bank$age_group, bank$y)

## interaction plots may worth looking?
interaction.plot(bank$contact, bank$duration, bank$y) # lowest p-val
interaction.plot(bank$age_group, bank$poutcome, bank$y) # somewhat high p-val

## Interaction plots testing continued
#interaction.plot(bank$job, bank$poutcome, bank$y)
interaction.plot(x.factor = bank$contact,    # variable to plot on x-axis
                 trace.factor = bank$duration, # variable to specify "traces"; here, lines
                 response = bank$y,    # variable to plot on y-axis
                 fun = median,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Response Yes or No (1 or 0)",
                 xlab = "Contact Method",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Contact",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

interaction.plot(bank$age_group, bank$poutcome, bank$y)
interaction.plot(x.factor = bank$age_group,    # variable to plot on x-axis
                 trace.factor = bank$poutcome, # variable to specify "traces"; here, lines
                 response = bank$y,    # variable to plot on y-axis
                 fun = median,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Response Yes or No (1 or 0)",
                 xlab = "Age group",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Poutcome",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

interaction.plot(bank$housing, bank$poutcome, bank$y)
interaction.plot(x.factor = bank$housing,    # variable to plot on x-axis
                 trace.factor = bank$poutcome, # variable to specify "traces"; here, lines
                 response = bank$y,    # variable to plot on y-axis
                 fun = median,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Response Yes or No (1 or 0)",
                 xlab = "Housing",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "P-outcome",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

## Parallel lines meaning no interactions

library(MASS)

# Split the data into training and test set
set.seed(777)
training.samples <- bank$y %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- bank[training.samples, ]
test.data <- bank[-training.samples, ]

training.samples2 <- bank2$y %>% 
createDataPartition(p = 0.8, list = FALSE)
train.data2  <- bank2[training.samples2, ]
test.data2 <- bank2[-training.samples2, ]

# Fit the full model 
str(bank)
full.model <- glm(y ~., data = train.data, family = binomial(link=logit))
summary(full.model)
full.model
full.model2 <- glm(y ~., data = train.data2, family = binomial(link=logit))

# Step model
library(MASS)
step.model <- full.model %>% stepAIC(direction = "forward", trace = F)
summary(step.model)
step.model$anova

step.model2 <- full.model2 %>% stepAIC(direction = "forward", trace = F)
summary(step.model2)
step.model2$anova

step.modelB <- full.model2 %>% stepAIC(direction = "backward", trace = FALSE)
summary(step.modelB)
step.modelB$anova

step.modelFl <- full.model2 %>% stepAIC(direction = "both", trace = F)
summary(step.modelFl)
step.modelFl$anova

## Interaction and without interaction
modelf <- glm(y ~ job+education+housing+loan+contact+month+duration+campaign+previous+poutcome+age_group, data = train.data2, family = binomial(link=logit))
modeln <- glm(y ~ (job+education+housing+loan+contact+month+duration+campaign+previous+poutcome+age_group)^2, data = train.data2, family = binomial(link=logit))
summary(modelf)
summary(modeln)

# likelihood test
library(lmtest)
lrtest(modelf, modeln)

library(car)
anovafit_f <- anova(modelf)
anovafit_n <- anova(modeln)
anovafit_n

summary(anovafit_f)

## Waldtest
waldtest(modelf, modeln)
waldtest(modelf)
waldtest(modeln)
anova(modelf, modeln)

# Lets consider the 3 variables for purposeful selection as following:
# Age, Duration, Month. The rationale for selecting these 3 variables are, to study if these 3 age, duration,
# and month of calling are significant predictor in predicting if the customer will subscribe to the 'term deposit' product
# being marketed by the bank represented by the response variable 'y'.

bank1 <- bank[,c(1,11,12,17)]
bank1$y <- as.factor(bank1$y)
summary(bank1$y)
# Make note: the dataset is biased

# lets split the data into training and validation in 80 / 20 ratio

library(caTools)
set.seed(150)
split = sample.split(bank1, SplitRatio = 0.80)

# Create training and testing sets
train = subset(bank1, split == TRUE)
test = subset(bank1, split == FALSE)
library(vtable)
test.data2$y <- as.factor(test.data2$y)
st(test.data2)

dim(train.data2); dim(test.data2)

summary(test.data2$y)

#### TOTALS :::: The test data has 780 observations for y=0, 124 for y=1     #########

# Computing Full Logistic regression model:

full.model <- glm(y ~., data = train, family = binomial(link = logit))
summary(full.model)

# Observation from full model:
# The full model has high significance for duration, the alpha of variable age is very close to 0.05
# and the variable month is found to have no significance in predicting the response

# computing step model.
# for the 3 variables selected, the purposeful selection is carried out using the backward elimination 
# method

library(MASS)
step.model <- full.model %>% step(trace = 1)   # Backward selection
summary(step.model)

# in the first model, all 3 variables are chosen, in the second iteration, the variable month
# which was found to be insignificant was removed by the step() function. the final step model
# consists of age and duration as the predictors

###########################################################

# COMPARING FULL, STEPWISE AND INTERACTION MODEL

###########################################################

# Evaluation of the Full model:

predicted <- predict(modelf, test.data2, type="response")

#find optimal cutoff probability to use to maximize accuracy

install.packages("InformationValue")
library(InformationValue)

optimal <- optimalCutoff(test.data2$y, predicted)[1]
# the optimal cut off for differentiating the response values is found to be 0.4 in full model
optimal

#create confusion matrix
confusionMatrix(test.data2$y, predicted)

#calculate sensitivity
sensitivity(test.data2$y, predicted)

#calculate specificity
specificity(test.data2$y, predicted)


#calculate total misclassification error rate
misClassError(test.data2$y, predicted, threshold=optimal)

sensitivity(test.data2$y, predicted)
specificity(test.data2$y, predicted)
misClassError(test.data2$y, predicted, threshold=optimal)


# Evaluation of the Full model:

predicted <- predict(modelf, test.data2, type="response")

#find optimal cutoff probability to use to maximize accuracy

install.packages("InformationValue")
library(InformationValue)

optimal <- optimalCutoff(test.data2$y, predicted)[1]
# the optimal cut off for differentiating the response values is found to be 0.4 in full model
optimal

#create confusion matrix
confusionMatrix(test.data2$y, predicted)

#calculate sensitivity
sensitivity(test.data2$y, predicted)

#calculate specificity
specificity(test.data2$y, predicted)


#calculate total misclassification error rate
misClassError(test.data2$y, predicted, threshold=optimal)

sensitivity(test.data2$y, predicted)
specificity(test.data2$y, predicted)
misClassError(test.data2$y, predicted, threshold=optimal)


# Evaluation of the Full model:

predicted2 <- predict(modeln, test.data2, type="response")

#find optimal cutoff probability to use to maximize accuracy

install.packages("InformationValue")
library(InformationValue)

optimal <- optimalCutoff(test.data2$y, predicted2)[1]
# the optimal cut off for differentiating the response values is found to be 0.5 in full model
optimal

#create confusion matrix
confusionMatrix(test.data2$y, predicted2)

#calculate sensitivity
sensitivity(test.data2$y, predicted2)

#calculate specificity
specificity(test.data2$y, predicted2)

#calculate total misclassification error rate
misClassError(test.data2$y, predicted2, threshold=optimal)

sensitivity(test.data2$y, predicted2)
specificity(test.data2$y, predicted2)
misClassError(test.data2$y, predicted2, threshold=optimal)

## ROC

class <- modelf$y
score <- modelf$fitted.values

library(ROCit)

measure <- measureit(score = score, class = class,
                     measure = c("ACC", "SENS", "FSCR"))

plot(measure$ACC~measure$Cutoff, type = "l")

roc_binormal <- rocit(score = bank2$balance, 
                      class = bank2$y,
                      method = "bin") 

plot(roc_binormal, YIndex = F, 
     values = F, col = c(2,4))

rocit_emp <- rocit(score = score, 
                   class = class, 
                   method = "emp")
rocit_bin <- rocit(score = score, 
                   class = class, 
                   method = "bin")
rocit_non <- rocit(score = score, 
                   class = class, 
                   method = "non")

plot(rocit_emp, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
lines(rocit_bin$TPR~rocit_bin$FPR, 
      col = 2, lwd = 2)
lines(rocit_non$TPR~rocit_non$FPR, 
      col = 4, lwd = 2)
legend("bottomright", col = c(1,2,4),
       c("Empirical ROC", "Binormal ROC",
         "Non-parametric ROC"), lwd = 2)
