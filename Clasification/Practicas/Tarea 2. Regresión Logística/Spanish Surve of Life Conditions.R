# ASSIGNMENT 2. LOGISTIC REGRESSION

# Author: Arturo Sánchez Palacio
# Date: 9/XII/18

# The assignment works on a database extracted from 2016 Spanish Survey of Life Conditions. 
# The goal of this assignment is to find out which families are in risk of poverty and why. (Which variables are responsible?)

# Firstly, we load the data in R:

setwd("/Users/arturosanchezpalacio/Documents/CUNEF/Clasificación/Tareas/Tarea 2. Regresión Logística") #We set the working directory

library(openxlsx) #This library is required to read from an Excel file.
data <- read.xlsx("data.xlsx") #We charge the rough data and start working on it.

# We give the variables proper names:

names(data) <- c("ID", "Aid", "Minors_rent", "Holidays", "Unexpected_expenses", "TV", "Computer", "Ends_meet", 
                 "Home", "Members", "Rent", "Poverty_risk", "Region", "Age_older", "Working_hours", "Adults", 
                 "Gender_older", "Occupation")

# We have a first glance at the data:

dim(data) #We have 418 homes in this study in which 18 variables are observed.
str(data) #All data has been loades as numeric.

# The ID does not give any interesting information:

data <- data[,-1]

# Poverty_risk is the variable we are going to try to classify:

data$Poverty_risk <- factor(data$Poverty_risk, levels = c(1,0), labels = c("Sí", "No"))

# This index is built upon the rent so it makes no sense to use the rent to predict it.  (It would be 
# a circular definition.  We erase this variable too)

data <- data[, -10]

# Some variables must be translated to factors:

data$Aid_D <- ifelse(data$Aid > 0, 1, 0)
data$Aid_D <- as.factor(data$Aid_D)
data$Holidays <- as.factor(data$Holidays)
data$Unexpected_expenses <- as.factor(data$Unexpected_expenses)
data$TV <- as.factor(data$TV)
data$Computer <- as.factor(data$Computer)
data$Ends_meet <- as.factor(data$Ends_meet)
data$Home <- as.factor(data$Home)
data$Occupation <- as.factor(data$Occupation)

data$Gender_older <- factor(data$Gender_older, levels = c(0,1), labels = c("Mujer", "Hombre"))
data$Region <- factor(data$Region, levels = c("ES21", "ES42", "ES52", "ES61", "ES41", "ES43", "ES53", "ES51",
                                              "ES11", "ES23", "ES30", "ES62", "ES22", "ES12", "ES70", "ES13", "ES24", "ES64", "ES63"),
                      labels = c("País Vasco", "Castilla la Mancha", "C. Valenciana", "Andalucía", "Castilla León",
                                 "Extremandura", "Baleares", "Cataluña", "Galicia", "Rioja", "Madrid", "Murcia",
                                 "Navarra", "Asturias", "Canarias", "Cantabria", "Aragón", "Melilla", "Ceuta"))

str(data) #Now the data seems like something we can work with

sapply(data,function(x) sum(is.na(x))) #There are no unknown values in this dataset.


# Now we are going to try to find and justify which variables will be used in the construction of the logistic regression:
# In order to do that we use Anova and Chi-Squared tests on contingency tables.
corr_pov_members <- lm(data$Members ~ data$Poverty_risk, data = data)
anova(corr_pov_members)

library(vcd)

tab <- xtabs(~ data$Home + data$Poverty_risk, data = data)
summary(assocstats(tab))

tab1 <- xtabs(~ data$Occupation + data$Poverty_risk, data = data)
summary(assocstats(tab1))

tab2 <- xtabs(~ data$Ends_meet + data$Poverty_risk, data = data)
summary(assocstats(tab2))

tab3 <- xtabs(~ data$Unexpected_expenses + data$Poverty_risk, data = data)
summary(assocstats(tab3))

tab4 <- xtabs(~ data$TV + data$Poverty_risk, data = data)
summary(assocstats(tab4))

tab5 <- xtabs(~ data$Computer + data$Poverty_risk, data = data)
summary(assocstats(tab5))

tab6 <- xtabs(~ data$Aid_D + data$Poverty_risk, data = data)
summary(assocstats(tab6))


# We set a seed in order to be able to replicate the experiment:

set.seed(1234)

# We split the data into two chunks: training and testing set. 
# The training set will be used to fit our model which we will be testing over the testing set.

train <- sample(nrow(data), 0.7*nrow(data))

data.train <- data[train,]

data.validate <- data[-train,]

#We check that the samples are balanced:

table(data.train$Poverty_risk)
table(data.validate$Poverty_risk)

model1 <- glm(Poverty_risk ~.,family = binomial(link = 'logit'),data = data.train)

summary(model1)

#Regions don't seem important at all. How troubled is to make ends meet, the occupation and the number of members
#seem like essential facts.

model2 <- glm(Poverty_risk ~ Occupation + Members + Ends_meet,family = binomial(link = 'logit'),data = data.train)
summary(model2)

#The AIC is lower so by simplifying the regression we have also improved it.

model3 <- glm(Poverty_risk ~ Occupation + Members + Ends_meet + Unexpected_expenses,family = binomial(link = 'logit'),data = data.train)
summary(model3)

#Unexpected expenses seem like an interesting fact and they lower the AIC so it's good.
#The biggest indicator is when families are able to overcome unexpected expenses.

model4 <- glm(Poverty_risk ~ Occupation + Members + Ends_meet + Unexpected_expenses + Home,family = binomial(link = 'logit'),
              data = data.train)
summary(model4)

# As a last idea receiving an aid (independently on how big it is) seems like an important factor. However it looks like it is not: 

model5 <- glm(Poverty_risk ~ Occupation + Members + Ends_meet + Unexpected_expenses + Home + Aid_D,
              family = binomial(link = 'logit'),
              data = data.train)
summary(model5)

#Home also appears to be an interesting variable. (Appears with significance in the case of renting).
#This will be our definite model.

anova(model4, test = "Chisq")
# The bigger the deviance, the more important the variable is. So in order to classify, first the occupation, then the
# ability to make ends meet, if the home is owned or rented, the number of members of the family and last the ability to 
# face unexpected expenses.

# As we can see, all the p-values are significant so all the variables are useful in order to explain the variable.

library(pscl)
pR2(model1)
pR2(model2)
pR2(model3)
pR2(model4)

fitted.results <- predict(model4, newdata = data.validate, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- factor(fitted.results, levels = c(0,1), labels = c("Sí", "No"))
(logit.perf <- table(data.validate$Poverty_risk, fitted.results, dnn = c("Actual", "Predicted")))

print(paste('Accuracy',sum(diag(logit.perf))/sum(logit.perf)))

# ROC curve
library(pROC)
resRoc <- roc(data.train$Poverty_risk ~ model4$fitted.values)
plot(resRoc, legacy.axes = TRUE)


# Area Under the Curve (AUC)

library(rms)

resLrm <- lrm(formula = Poverty_risk ~ Occupation + Members + Ends_meet + Unexpected_expenses + Home,
              data    = data.validate,
              x = TRUE, y = TRUE)
resLrm








