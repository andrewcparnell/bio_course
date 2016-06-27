# Code to run the South African Heart rate data set

# Load in the data
SA = read.table('https://raw.githubusercontent.com/andrewcparnell/bio_course/master/code/SAheartdata.txt', header=TRUE, sep = ',', row.names = 1)

# Look at the first few rows:
head(SA)

# Variables:
# sbp		systolic blood pressure
# tobacco		cumulative tobacco (kg)
# ldl		low densiity lipoprotein cholesterol
# adiposity  approx percentage body fat
# famhist		family history of heart disease (Present, Absent)
# typea		type-A behavior
# obesity  obesity measure
# alcohol		current alcohol consumption
# age		age at onset
# chd		response, coronary heart disease

# Create a plot
par(mfrow=c(2,2)) # This command creates a 2 by 2 panel for plotting
boxplot(age ~ chd, data = SA, xlab = 'chd', ylab = 'age')
boxplot(tobacco ~ chd, data = SA, xlab = 'chd', ylab = 'tobacco')
boxplot(alcohol ~ chd, data = SA, xlab = 'chd', ylab = 'alcohol')
boxplot(obesity ~ chd, data = SA, xlab = 'chd', ylab = 'obesity')
par(mfrow=c(1,1)) # This command resets the plotting window

## TASK: Try some other variables in the boxplots

# Fit some classification models ------------------------------------------

# Fit a model of chd vs age
model_1 = glm(chd ~ age, data = SA, family = 'binomial')
summary(model_1)

# Add an extra explanatory variable and an interaction
model_2 = glm(chd ~ age + adiposity + age:adiposity, data = SA, family = 'binomial')
summary(model_2)

# Create a misclassification table for probability cut-off 0.5
table(SA$chd, round(model_1$fitted.values), dnn=c('True','Predicted'))

## QUESTION: What does the round function do? Why is the round statement equivalent to choosing a probability cut-off of 0.5?

# Find another cut-off
cut_off = 0.3
# The next line contains some clever code, in particular the part as.integer(model_1$fitted.values>cut_off)
# Try running these individual parts through the Console window to see what happens, e.g. just the part model_1$fitted.values, then just the part model_1$fitted.values>cut_off, then just the part as.integer(model_1$fitted.values>cut_off)
tab = table(SA$chd, 
            as.integer(model_1$fitted.values>cut_off), 
            dnn=c('True','Predicted'))
cat('Sensitivity = ',tab[2,2]/(tab[2,1] + tab[2,2]),
    'Specificity = ',tab[1,1]/(tab[1,1] + tab[1,2]))

## TASK: Try changing the cut off value and look at the effect on the sens and spec

# Find the value that maximises Youden's index
# Again this is more complicated code - work through it slowly
prob_grid = seq(0.1, 0.6, length = 50) # Set up a sequence of probability values
youden = rep(NA, length = 50) # Set up a storage container of same length
for(i in 1:50) { # Loop through each probability sequence value
  # Calculate the misclassification table
  tab = table(SA$chd, 
              as.integer(model_1$fitted.values>prob_grid[i]))
  # Calculate sensitivity and specificity
  sens = tab[2,2]/(tab[2,1] + tab[2,2])
  spec = tab[1,1]/(tab[1,1] + tab[1,2])
  # Calculate youden's index
  youden[i] = sens + spec - 1
}
# Plot the output
plot(prob_grid, youden, type = 'l')

# TASK: An alternative to the youden index is the maximal efficiency metric defined as p*sens + (1-p)*spec. Implement this alternative metric and plot the output

# Create the ROC curve
library(pROC)
plot(roc(SA$chd, model_1$fitted.values))

## TASK: Try plotting the ROC curves of some of the other models. Can you see a difference? See if you can overplot the different models in different colours (hint: use add=TRUE)

# Find the AUC value
auc(SA$chd, model_1$fitted.values)

## TASK: Try calculating the AUC values for different models. See if you can find a model with the highest AUC values

# Calibration plot
library(ROCR)
pred = prediction(model_1$fitted.values, SA$chd)
acc = performance(pred, measure = 'acc')
plot(acc)
abline(a=0, b=1, col='red')


# Decision curve plot - not covered
# library(DecisionCurve)
# model_3 = decision_curve(chd ~ age,
#                          data = SA, 
#                          study.design = "cohort")
# 
# # plot the curve
# plot_decision_curve(model_3,  curve.names = "CHD ~ age", cost.benefit.axis = FALSE)

