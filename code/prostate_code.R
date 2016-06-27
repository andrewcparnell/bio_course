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

# Create a plot
pairs(prostate)

# Look at the relationship between weight and lpsa
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(prostate$lweight, prostate$lpsa, xlab = 'log(weight)', ylab= 'log(psa)')

# A first statistical model
model_1 = lm(formula = lpsa ~ lweight, data = prostate)
summary(model_1)

# Two explanatory variables
model_2 = lm(formula = lpsa ~ lweight + age, data = prostate)
summary(model_2)

# All of them - but get rid of training
model_3 = lm(formula = lpsa ~ . - train, data = prostate)
summary(model_3)

# A model with interactions
model_4 = lm(formula = lpsa ~ lweight + age + lweight:age, data = prostate)
summary(model_4)

