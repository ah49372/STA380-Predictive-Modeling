x = cleanedData
colnames(x)
attach(x)
# total number of stroke in our dataset 
table(x$stroke)
table(x$gender)

# make train and test dataset for logistic regression 
set.seed(1)

train = sample(c(TRUE,FALSE), nrow(x), rep = TRUE)
test= (!train)               

xtrain = x[train,]
xtest = x[test,]
xtrain = xtrain[1:nrow(xtest),]


table(xtrain$stroke)
table(xtest$stroke)

lrfit = glm(stroke~., data = x, family = binomial, subset = train)
#age+hypertension+heart_disease+avg_glucose_level
summary(lrfit)

lrfitprobs = predict(lrfit, xtest, type = "response")

lrfitpredictions = rep(0, length(lrfitprobs))
lrfitpredictions[lrfitprobs > 0.05] = 1

# confustion matrix 

conf = table(lrfitpredictions, xtest$stroke)
conf

