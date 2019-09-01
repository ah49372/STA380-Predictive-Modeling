# Logistic Regression Model 

rm(list=ls())
x = read.csv("C:/Users/josh/Desktop/cleanedData.csv")
attach(x)

# Split dataset to train and test 
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(x), rep = TRUE)
test= (!train)               

xtrain = x[train,]
xtest = x[test,]

lrfit = glm(stroke~., data = x, family = binomial, subset = train)

summary(lrfit)

lrfitprobs = predict(lrfit, xtest, type = "response")
lrfitpredictions = rep("no_stroke", length(lrfitprobs))
lrfitpredictions[lrfitprobs > 0.03] = "stroke"

# confustion matrix 
table (xtest$stroke, lrfitpredictions)
