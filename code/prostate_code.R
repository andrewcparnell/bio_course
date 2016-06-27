# Code to run the Prostate cancer data set

# Load in the data
prostate = read.csv('https://raw.githubusercontent.com/andrewcparnell/bio_course/master/code/prostate.csv')

# Look at the first few rows:
head(prostate)

# Columns are:
# lcavol - log(cancer volume)
# lweight - log(weight)
# age - age
# lbph - log(benign prostatic hyperplasia amount)
# svi - seminal vesicle invasion
# lcp - log(capsular penetration)
# gleason - grade of cancer
# pgg45 - percentage Gleason scores 4 or 5
# lpsa - outcome - log prostate specific antigen
# train - whether the observation should be included in the training or test set
# From Stamey et al (1989)

# Create a matrix scatter plot
# If this looks too small, click on the zoom button to get a bigger version
pairs(prostate)

## QUESTION: can you use square brackets to select only a few of the columns for the plot?

# Look at the relationship between weight and lpsa
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
with(prostate, plot(lweight, lpsa, xlab = 'log(weight)', ylab= 'log(psa)'))

# Run some regression models ----------------------------------------------

# A first statistical model
model_1 = lm(formula = lpsa ~ lweight, data = prostate)
summary(model_1)

## QUESTION: try changing the explanatory variable from lweight to lcavol. Does the model get better?

# Two explanatory variables
model_2 = lm(formula = lpsa ~ lweight + age, data = prostate)
summary(model_2)

# Now use all of them - but get rid of training as it's not helpful
# Note the format of the formula: response ~ explanatory + other variables - removed variables
# The . means include all remaining variables
model_3 = lm(formula = lpsa ~ . - train, data = prostate)
summary(model_3)

## QUESTION: which of the following three models do you think is best?

# A model with interactions
model_4 = lm(formula = lpsa ~ lweight + age + lweight:age, data = prostate)
summary(model_4)

## Task: Try some other interactions

# Diagnostics -------------------------------------------------------------

# Check the residuals:
hist(model_1$residuals, freq=FALSE)
curve(dnorm(x, sd = 1.046), col='red', add = TRUE)     

## QUESTION: What does the curve function do? Try changing the colours

# QQ plot
qqnorm(model_1$residuals)
qqline(model_1$residuals)

# Predictions vs fits
plot(prostate$lpsa, model_1$fitted.values,
     ylab = 'Predicted values', xlab = 'True values')
abline(a=0, b=1, col='red')
# The abline function is useful for drawing straight lines

## QUESTION: what do the a and b arguments of abline do?

# 5-fold cross validation - this is more complicated code
# It's worth spending some time trying to work out what's happening here
# There are lots of new functions here so don't worry if you don't follow it all
n_folds = 5 # Number of folds
n = nrow(prostate) # Number of observations
folds = sample(1:5, n, replace = TRUE) # Create the folds
out_of_sample_preds = rep(NA, length = n) # Create a holder to store the output
for(i in 1:n_folds) { # Loop through the folds
  # Create a model by leaving out fold i
  curr_model = lm(lpsa ~ lweight, data = subset(prostate, folds!=i))
  # Predicted the values for fold i
  out_of_sample_preds[folds==i] = predict(curr_model, newdata = subset(prostate, folds==i))
}

## TASK: Try changing the number of folds and check that the plot still runs.

# Plot the out of sample predictions
plot(prostate$lpsa, out_of_sample_preds,
     ylab = '5-fold CV Predicted values', xlab = 'True values')
abline(a=0, b=1, col='red')

## QUESTION: Does the out of sample plot improve if you use lcavol instead of lweight?

# Model comparison with AIC -----------------------------------------------

# Model selection via stepAIC from the MASS package
library(MASS)
model_5 = stepAIC(model_4)
summary(model_5)

## TASK: see if you can follow the output from the stepAIC function
## QUESTION: What happens if you start with a model that includes interactions?
