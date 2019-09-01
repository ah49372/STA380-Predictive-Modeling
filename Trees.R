library (MASS)
library(randomForest)
library(tree)
set.seed(1)
library(readr)
Stroke <- read_csv("Project/cleanedData.csv")
View(Stroke)
train<-sample(1:nrow(Stroke), nrow(Stroke)/2)
Stroke.train <- Stroke[train, ]
Stroke.test <- Stroke[-train, ]
reg.tree = tree(stroke~., data=Stroke, subset=train)
reg.tree=tree(stroke~., data=Stroke.train)
summary(reg.tree)
plot(reg.tree)
text(reg.tree,pretty=0)

------------------------------
  
library(ggplot2)
library(dplyr)

Stroke <- read.csv("Project/cleanedData.csv")
summary(Stroke)
Stroke <- Stroke %>% filter(gender !='Other') %>% select(-id)


#convert some variables to factors
Stroke$gender <- as.factor(Stroke$gender)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$stroke <- as.factor(Stroke$stroke)

attach(Stroke)

ggplot(Stroke, aes(x=stroke)) + 
  geom_bar() + 
  theme_bw()

#First question? Which factor is related to stroke(just to test single factor)
#gender?
ggplot(Stroke, aes(x=gender, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'gender',
       title ='gender distribution for stroke')

#age?
ggplot(Stroke, aes(x=age, fill=stroke)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  labs(y = 'stroke people count',
       x = 'age',
       title ='age distribution for stroke')

#hypertension
ggplot(Stroke, aes(x=hypertension, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'hypertension',
       title ='hypertension distribution for stroke')

#heart_disease
ggplot(Stroke, aes(x=heart_disease, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'heart_disease',
       title ='heart_disease distribution for stroke')

percent_heart_disease=prop.table(table(Stroke$stroke, Stroke$heart_disease))
percent_heart_disease[2, ] / percent_heart_disease[1, ] 



#ever_married
ggplot(Stroke, aes(x=ever_married, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'ever_married',
       title ='ever_married distribution for stroke')

percent_ever_married=prop.table(table(Stroke$stroke, Stroke$ever_married))
percent_ever_married[2, ] / percent_ever_married[1, ] #

#work_type
ggplot(Stroke, aes(x=work_type, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'work_type',
       title ='work_type distribution for stroke')

percent_work_type =prop.table(table(Stroke$stroke, Stroke$work_type))
percent_work_type[2, ] / percent_work_type[1, ]

#Residence_type
#No difference
ggplot(Stroke, aes(x=Residence_type, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'Residence',
       title ='Residence distribution for stroke')

percent_Residence_type =prop.table(table(Stroke$stroke, Stroke$Residence_type))
percent_Residence_type[2, ] / percent_Residence_type[1, ]

#avg_glucose_level
#eyeballing: no difference
ggplot(Stroke, aes(x=avg_glucose_level, fill=stroke)) +
  theme_bw() +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  labs(y = 'stroke people count',
       x = 'avg_glucose_level',
       title ='Glucose distribution for stroke')

#bmi
#eyeballing: no difference
ggplot(Stroke, aes(x=bmi, fill=stroke)) +
  theme_bw() +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  labs(y = 'stroke people count',
       x = 'bmi_glucose_level',
       title ='bmi distribution for stroke')

#Age with gender together
ggplot(Stroke, aes(x = age, fill=stroke)) + 
  theme_bw() + 
  facet_wrap(~gender) +
  geom_histogram(binwidth = 5) +
  labs(y ='Passenger Count',
       x = 'Age (binwidth = 5)', 
       title = 'Titanic Age Distribution')


#################
# distribution
distribution_ratio  <- function(x){
  ## Function creates a stacked bar plots for stroke outcome and ratio by binomial variables (0/1)  
  plt <- Stroke %>% 
    select(stroke, x, gender) %>%
    group_by(stroke, var = eval(parse(text = x))) %>% 
    summarise(count = length(gender)) %>%
    group_by(stroke) %>%
    mutate(ratio = round(count*100/sum(count), 1)) %>%
    ggplot(aes(y = ratio, x = stroke, fill = var)) + 
    geom_bar(stat="identity") +
    labs(title=paste0("Ratio of stroke outcome by ", x), fill = x) +
    theme_bw()
  return(plt)
}
distribution_ratio("hypertension")
distribution_ratio("ever_married")
distribution_ratio("Residence_type")
distribution_ratio("work_type")
distribution_ratio("smoking_status")

# Project Model
knitr::opts_chunk$set(echo = TRUE)
Stroke = read.csv("Project/cleanedData.csv")
Stroke$gender <- as.factor(Stroke$gender)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$stroke <- as.factor(Stroke$stroke)

Stroke = Stroke[,-c(1, 8)]
attach(Stroke)

library(rpart)
library(randomForest)
library(gbm)

set.seed(123)
n = nrow(Stroke)
n1 = floor(n/2) #length of train
n2 = floor(n/4) #length of validation
n3 = n - n1 - n2
new_order = sample(1:n, n)
Stroketrain = Stroke[new_order[1:n1],]
Strokeval = Stroke[new_order[n1+1:n2],]
Stroketest = Stroke[new_order[n1+n2+1:n3],]

big.tree = rpart(stroke ~.,method="class",data=Stroketrain,
                 control=rpart.control(minsplit=5,cp=.0001)
)
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

cpvec = big.tree$cptable[,"CP"] #cp values to try
ntree = length(cpvec) #number of cv values = number of trees fit.
iltree = rep(0,ntree) #in-sample loss
oltree = rep(0,ntree) #out-of-sample loss
sztree = rep(0,ntree) #size of each tree
temptree = prune(big.tree,cp=cpvec[1],type='class')
for(i in 1:ntree) {
  cat('tree i: ',i,'\n')
  temptree = prune(big.tree,cp=cpvec[i])
  ifit = predict(temptree, Stroketrain, type = 'class')
  il_table = table(ifit, Stroketrain$stroke)
  iltree[i] = (il_table[1] + il_table[4])/nrow(Stroketrain)
  sztree[i] = length(unique(temptree$where))
  ofit = predict(temptree,Strokeval,type = 'class') #use val to predict
  ol_table = table(ofit, Strokeval$stroke)
  oltree[i] = (ol_table[1] + ol_table[4])/nrow(Strokeval)
}

rgl = range(c(iltree,oltree))
plot(range(sztree),rgl,type='n',xlab='tree size',ylab='correction rate')
points(sztree,iltree,pch=15,col='red')
points(sztree,oltree,pch=16,col='blue')
legend("topright",legend=c('in-sample','out-of-sample'),lwd=3,col=c('red','blue'))

plot(temptree$variable.importance)

iitree = which.min(oltree)
thetree = prune(big.tree,cp=cpvec[iitree])
thetreepred = predict(thetree,Stroketest, type='class')
test_table = table(thetreepred, Stroketest$stroke)
correction_rate = (test_table[1] + test_table[4])/nrow(Stroketest)
test_table
correction_rate

# Random Forest
library(randomForest)
p = ncol(Stroketrain) - 1
mtryv = c(p, floor(sqrt(p)))
ntreev = c(100,500)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree') #rename the parmrf
nset = nrow(parmrf) 
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(stroke~.,data=Stroketrain,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=Strokeval)
  if_table = table(ifit, Stroketrain$stroke)
  ol_table = table(ofit, Strokeval$stroke)
  ilrf[i] = sum(diag(if_table))/nrow(Stroketrain)
  olrf[i] = sum(diag(ol_table))/nrow(Strokeval)
  rffitv[[i]]=temprf
}

print (cbind(parmrf, ilrf, olrf))

################################

# Boosting

Stroke <- read_csv("Project/cleanedData.csv")
View(Stroke)

Stroke$gender <- as.factor(Stroke$gender)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$smoking_status<-as.factor(Stroke$smoking_status)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$stroke <- as.factor(Stroke$stroke)
Stroke = Stroke[,-c(1, 8)]

train <- sample(1:nrow(Stroke), nrow(Stroke)/2)
s.train = Stroke[train, ]
s.test=Stroke[-train, ]
library(gbm)
set.seed(1)
boost.stroke = gbm(stroke ~ ., data=s.train, distribution="gaussian", n.trees=1000, shrinkage=0.01)
summary(boost.stroke)

boost.fit <- gbm(stroke~., data=s.train, distribution = 'bernoulli',n.trees = 5000)
boost.probs <- predict(boost.fit, newdata=s.test, n.trees=5000)
boost.pred<-ifelse(boost.probs>0.05, 1, 0)
table(s.test$stroke, boost.pred)

# Bagging
library(randomForest)
set.seed(1)
bag.stroke = randomForest(stroke~.,data = s.train,mtry=10,importance=TRUE)
bag.stroke
predict.bag <- predict(bag.stroke,newdata = s.test)
mean((predict.bag - s.test$stroke)^2)
importance(bag.stroke)


#KNN
library(class)
library(kknn)
