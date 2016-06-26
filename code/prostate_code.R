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
