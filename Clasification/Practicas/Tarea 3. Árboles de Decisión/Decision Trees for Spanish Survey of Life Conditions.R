# ASSIGNMENT 3. DECISION TREES

# Author: Arturo Sánchez Palacio
# Date: 29/XII/18

# The assignment works on a database extracted from 2016 Spanish Survey of Life Conditions. 
# The goal of this assignment is to find out which families are in risk of poverty and why.
# (Which variables are responsible?) using decision trees.

# Firstly, we load the data in R:

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
summary(data) #To get an idea about how the data is distributed.

# We set a random seed in order to make the experiments replicable:

set.seed(12345)

#Firstly we are going to build a training and a test sample:

train <- sample(nrow(data), 0.7*nrow(data)) #We create a training sample with 70% of the data. 

data.train <- data[train,]
data.validate <- data[-train,] #The remaining observations are stored for validation 

#Now we must check that the samples are balanced:

table(data.train$Poverty_risk)
table(data.validate$Poverty_risk)

# Sample are well balanced so we can use them to build the decision tree.

# In this experiment we are going to consider two different options.

#OPTION A) Buiding a tree using all the variables in the dataframe (both standard tree and inference tree).
#OPTION B) Building a tree using the variables that were considered as relevant in the Spanish Survey of Life Conditions report handed before.

#In order to perform both options we are going to require the following libraries:

library(rpart) #Creates the tree objects
library(rpart.plot) #Helps plotting the trees.
library(partykit) #Another way of plotting trees
library(party) #Used to build Condition Inference Trees


############################### OPTION A ###############################

# Let's start with option A since it is the most general:

#We start estimating the tree:

general_tree <- rpart(Poverty_risk ~ ., data = data.train, method = "class",
                      parms = list(split = "information"))

print(general_tree)

summary(general_tree)

plot(general_tree, uniform = TRUE, branch = 0.4)

text(general_tree, use.n = TRUE, cex = 0.75)

# We display a table with the parametrical complexity:

general_tree$cptable

# Plotting the CP curve:

plotcp(general_tree)

#Now we must prune the tree according to the complexity parametre. 
#We must choose the CP associated to the least xerror in this case is the third one: CP = 0.03759398

pruned_general_tree <- prune(general_tree, cp = 0.03759398)

#The following is a great and clear representation:

prp(pruned_general_tree, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "General Decision (Pruned) Tree")

#The following representation is not that good:

plot(as.party(pruned_general_tree))

#Now we are going to predict over the validation set in order to have a way to compare the different trees:

pred_general_treea <- predict(general_tree, data.validate, type = "class")

general_pred_tablea <- table(data.validate$Poverty_risk, pred_general_treea,
                             dnn = c("Actual", "Predicted"))

general_pred_tablea

pred_general_tree <- predict(pruned_general_tree, data.validate, type = "class")

general_pred_table <- table(data.validate$Poverty_risk, pred_general_tree,
                            dnn = c("Actual", "Predicted"))

general_pred_table

#Once we have seen the results of a standard tree let's build and plot a conditional inference tree:

general_inference_tree <- ctree(Poverty_risk~., data = data.train)
plot(general_inference_tree, main = "General Conditional Inference Tree")

# We compute the predictions for the validation set based on this new tree:

pred_conditional_general_tree <- predict(general_inference_tree, data.validate, type = "response")

general_conditional_pred_table <- table(data.validate$Poverty_risk, pred_conditional_general_tree,
                                        dnn = c("Actual", "Predicted"))

general_conditional_pred_table


#The two trees obtained here yield an accuracy of 69.4% and 72.2%. There are not bad results but let's check what we get
# from performing variable selection.

############################### OPTION B ###############################

#We start estimating the tree using only the selected variables ie, :

improved_tree <- rpart(Poverty_risk ~Occupation + Members + Ends_meet + Unexpected_expenses + Home, data = data.train, method = "class",
                       parms = list(split = "information"))

print(improved_tree)

summary(improved_tree)

# We display a table with the parametrical complexity:

improved_tree$cptable

# Plotting the CP curve:

plotcp(improved_tree)

#Now we must prune the tree according to the complexity parametre. 
#We must choose the CP associated to the least xerror in this case is the fourth one: CP =  0.01503759

pruned_improved_tree <- prune(general_tree, cp =  0.01503759)

#The following is a great and clear representation:

prp(pruned_improved_tree, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "Decision Tree (with selected variables)")

# If we liked something a bit more fancy we can use:

rpart.plot(pruned_improved_tree, 
           box.palette = "GnBu",
           branch.lty = 6, shadow.col = "gray", nn = TRUE,
           main = "Árbol de Decisión (con selección de variables)")

#The following representation is again not that good:

plot(as.party(pruned_improved_tree))

#Now we are going to predict over the validation set in order to have a way to compare the different trees:


pred_improved_treeb <- predict(improved_tree, data.validate, type = "class")

improved_pred_tableb <- table(data.validate$Poverty_risk, pred_improved_treeb,
                              dnn = c("Actual", "Predicted"))

improved_pred_tableb

prp(improved_tree, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "General Decision Tree (with selected variables)")

rpart.plot(improved_tree, 
           box.palette = "GnBu",
           branch.lty = 3, shadow.col = "gray", nn = TRUE,
           main = "Árbol de decisión con variables seleccionadas")

pred_improved_tree <- predict(pruned_improved_tree, data.validate, type = "class")

improved_pred_table <- table(data.validate$Poverty_risk, pred_improved_tree,
                             dnn = c("Actual", "Predicted"))

improved_pred_table


#Once we have seen the results of a standard tree for selected variables let's build and plot a conditional inference tree:

improved_inference_tree <- ctree(Poverty_risk~., data = data.train)
plot(improved_inference_tree, main = "General Conditional Inference Tree")

# We compute the predictions for the validation set based on this new tree:

pred_conditional_improved_tree <- predict(improved_inference_tree, data.validate, type = "response")

improved_conditional_pred_table <- table(data.validate$Poverty_risk, pred_conditional_improved_tree,
                                         dnn = c("Actual", "Predicted"))

improved_conditional_pred_table


# Finally we can see that in general terms the inference tree yields better results for the validation set.
# For inference trees  the variable selection doesn't improve the results, however in the standard trees we can see a small 
# improvement when we perform variable selection.

######### OPTION C ##############

#In this section we are going to consider only 'objective' data. This idea is more developed on the paper but basically
#here we consider that maybe the variable measuring how difficult is to make ends meet and face unexpected expenses is 
# not that objective or well perceived by the surveyed. 

obj_data <- data[,c(1,2,3,5,6,8,9,10,11,12,13,14,15)]
train <- sample(nrow(obj_data), 0.7*nrow(obj_data)) #We create a training sample with 70% of the data. 

obj_data.train <- obj_data[train,]
obj_data.validate <- obj_data[-train,] #The remaining observations are stored for validation 


#We start estimating the tree:

obj_general_tree <- rpart(Poverty_risk ~ ., data = obj_data.train, method = "class",
                          parms = list(split = "information"))

print(obj_general_tree)

summary(obj_general_tree)

plot(obj_general_tree, uniform = TRUE, branch = 0.4)

text(obj_general_tree, use.n = TRUE, cex = 0.75)

# We display a table with the parametrical complexity:

obj_general_tree$cptable

# Plotting the CP curve:

plotcp(obj_general_tree)

#Now we must prune the tree according to the complexity parametre. 
#We must choose the CP associated to the least xerror in this case is the third one: CP = 0.03759398

pruned_obj_general_tree <- prune(obj_general_tree, cp = 0.03007519)

#The following is a great and clear representation:

prp(pruned_obj_general_tree, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "Objective Decision (Pruned) Tree")

#The following representation is not that good:

plot(as.party(pruned_general_tree))

#Now we are going to predict over the validation set in order to have a way to compare the different trees:

pred_obj_general_treea <- predict(obj_general_tree, data.validate, type = "class")

obj_general_pred_tablea <- table(data.validate$Poverty_risk, pred_obj_general_treea,
                                 dnn = c("Actual", "Predicted"))

obj_general_pred_tablea

obj_pred_general_tree <- predict(pruned_obj_general_tree, data.validate, type = "class")

obj_general_pred_table <- table(data.validate$Poverty_risk, obj_pred_general_tree,
                                dnn = c("Actual", "Predicted"))

obj_general_pred_table

#Once we have seen the results of a standard tree let's build and plot a conditional inference tree:

general_inference_tree <- ctree(Poverty_risk~., data = data.train)
plot(general_inference_tree, main = "General Conditional Inference Tree")

# We compute the predictions for the validation set based on this new tree:

pred_conditional_general_tree <- predict(general_inference_tree, data.validate, type = "response")

general_conditional_pred_table <- table(data.validate$Poverty_risk, pred_conditional_general_tree,
                                        dnn = c("Actual", "Predicted"))

general_conditional_pred_table

#This tree yields an accuracy of 72.22%.

