getwd()
x = read.csv("C:/Users/josh/Desktop/cleanedData.csv")
colnames(x)
attach(x)
# tota
# make train and tl number of stroke in our dataset 
table(x$stroke)
table(x$gender)
est dataset for logistic regression 
set.seed(1)

train = sample(c(TRUE,FALSE), nrow(x), rep = TRUE)
test= (!train)               


xtrain = x[train,]
xtest = x[test,]

table(xtrain$stroke)
table(xtest$stroke)

lrfit = glm(stroke~., data = x, family = binomial, subset = train)

summary(lrfit)

lrfitprobs = predict(lrfit, xtest, type = "response")

boxplot(lrfitprobs)

lrfitpredictions = rep("no_stroke", length(lrfitprobs))


lrfitpredictions[lrfitprobs > 0.03] = "stroke"

# confustion matrix 
table(lrfitpredictions, xtest$stroke)

