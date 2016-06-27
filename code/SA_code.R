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
par(mfrow=c(2,2))
boxplot(age ~ chd, data = SA, xlab = 'chd', ylab = 'age')
boxplot(tobacco ~ chd, data = SA, xlab = 'chd', ylab = 'tobacco')
boxplot(alcohol ~ chd, data = SA, xlab = 'chd', ylab = 'alcohol')
boxplot(obesity ~ chd, data = SA, xlab = 'chd', ylab = 'obesity')
par(mfrow=c(1,1))

# Fit a model of chd vs age
model_1 = glm(chd ~ age, data = SA, family = 'binomial')
summary(model_1)

# Add an extra explanatory variable and an interaction
model_2 = glm(chd ~ age + adiposity + age:adiposity, data = SA, family = 'binomial')
summary(model_2)

