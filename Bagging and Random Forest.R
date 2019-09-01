# Open the CSV data and change (gender, hypertension, heart_disease, ever_married, work_type,
Residence_type, stroke) as factor
Stroke = read.csv("cleanedData.csv")
Stroke$gender <- as.factor(Stroke$gender)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$stroke <- as.factor(Stroke$stroke)

Stroke = Stroke[,-c(1, 8)]
attach(Stroke)

#select 500 rows of stroke and 1000 rows of unstroke

train_stroke_full <- Stroke[Stroke$stroke==1,]
set.seed(123)
order1 <- sample(nrow(train_stroke_full), 500)
train_stroke <- train_stroke_full[order1, ]
test_stroke <- train_stroke_full[-order1, ]
train_nonstroke_full <- Stroke[Stroke$stroke==0, ]
order2 <- sample(nrow(train_nonstroke_full), 1000)
train_nonstroke <- train_nonstroke_full[order2, ]
test_nonstroke <- train_nonstroke_full[-order2, ]
train <- rbind(train_nonstroke, train_stroke)
test <- rbind(test_nonstroke, test_stroke)
order3 <- sample(nrow(test), floor(nrow(test)/2))
validation <- test[order3, ]
test <- test[-order3, ]
#####################################################################################################
# Random Forest

library(randomForest)
set.seed(456)
p=ncol(train)-1
mtryv = c(3)
ntreev = c(100,1000) # how many trees I want to grow;choose either 100 and 1000
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree') #rename the parmrf
nset = nrow(parmrf)  # figure out the attempt rows
olrf = rep(0,nset) #out of sample
ilrf = rep(0,nset) # in sample: which precision value I want to use
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(stroke~.,data=train,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=validation)
  olrf[i] = sum(table(validation$stroke, ofit)[2, 2])/nrow(validation[validation$stroke==1,])
  ilrf[i] = sum(table(train$stroke, ifit)[2, 2])/nrow(train[train$stroke==1,])
  rffitv[[i]]=temprf
}

iirf=which.max(olrf) # location of the max value: parameter of which one works the best
therf = rffitv[[iirf]] # access the best model
therfpred=predict(therf,newdata=test) # corresponding to iirf
correction_rate = sum(table(test$stroke, therfpred)[2, 2])/nrow(test[test$stroke==1,])
correction_rate # recall
table(test$stroke, therfpred)

# Variable Importance for Random Forest
varImpPlot(therf)
#############################################################################################
# Bagging

library(randomForest)
set.seed(1011)
p=ncol(train)-1
mtryv = c(p) #use maximum biggest one
ntreev = c(100,1000, 5000)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree') #rename the parmrf
nset = nrow(parmrf) 
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(stroke~.,data=train,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=validation)
  olrf[i] = sum(table(validation$stroke, ofit)[2, 2])/nrow(validation[validation$stroke==1,])
  ilrf[i] = sum(table(train$stroke, ifit)[2, 2])/nrow(train[train$stroke==1,])
  rffitv[[i]]=temprf
}

iirf=which.max(olrf)
bagging_mod = rffitv[[iirf]]
therfpred1=predict(bagging_mod,newdata=test)
correction_rate = sum(table(test$stroke, therfpred1)[2, 2])/nrow(test[test$stroke==1,])
correction_rate
table(test$stroke, therfpred1)

varImpPlot(bagging_mod)
