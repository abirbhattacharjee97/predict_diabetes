library(e1071)
library(mlbench)
data(PimaIndiansDiabetes)
#import data
data <- PimaIndiansDiabetes
str(data)
#data$diabeticf <- factor(data$diabetes)

#partition data
set.seed(1234)
pd <- sample( 2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]

#dt using rpart
library(rpart)
fit <- rpart(diabetes ~ . , method="class", data=train)
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
library(rpart.plot)
rpart.plot(fit)
plot(pfit)

#making decision tree predictor
library(party)
tree <- ctree(diabetes ~ pregnant+ glucose+ pressure + triceps + insulin + mass + pedigree + age, data = train )
plot(tree)

#making naive bayes predictor
nb.fit = naiveBayes(diabetes~., data=train)

#making svm 
svmmodel <- svm(diabetes ~ ., data = train,kernel="radial", cost = 4, gamma = 0.5)

n_pos <- 0;
n_neg <- 0;
eq <- 0;

#predicting with ensemble based on majority
for(x in 1:10)
{
  
  n_pos = n_neg =0;
  print( paste("Instance" , x))
  r1 = predict(nb.fit, PimaIndiansDiabetes[x,1:8])
  r2 = predict(tree, PimaIndiansDiabetes[x,1:8])
  r3 = predict(svmmodel, PimaIndiansDiabetes[x,1:8])
  dt2 = predict(pfit,validate[x,1:8])
  if(dt2[1]>dt2[2]){
    r4 = "neg"
  }else{
    r4 = "pos"
  }
  
  print(paste("Naive Bayes :", r1, "|  DT:", r2, "|  SVM:",r3, "|   DT2", r4))
  
  if( r1 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1 
  }
  
  if( r2 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1
  }
  
  if( r3 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1
  }
  
  print(paste("Num Pos: ", n_pos , "Num Neg: ", n_neg))
  if(n_pos > n_neg){
    print(paste("Ensemble Output: Positive "))
    print(" ", quote = FALSE)
  }else{
    print(paste("Ensemble Output: Negative "))
    print(" ", quote = FALSE)
  }
  
}

#misclassification of DT on validate data
test_pred <- predict(tree,validate)
tab_validate <- table(test_pred, validate$diabetes)
print(tab_validate)
m_dt <- sum(diag(tab_validate))/sum(tab_validate)
print(paste("Misclassification: " , (1-m_dt)*100 , "| Accuracy: ", m_dt*100))


#misclassification of Naive Bayes on validate data
test_pred <- predict(nb.fit,validate)
tab_validate <- table(test_pred, validate$diabetes)
print(tab_validate)
m_dt <- sum(diag(tab_validate))/sum(tab_validate)
print(paste("Misclassification: " , (1-m_dt)*100 , "| Accuracy: ", m_dt*100))


#misclassification of SVM on validate data
test_pred <- predict(svmmodel,validate)
tab_validate <- table(test_pred, validate$diabetes)
print(tab_validate)
m_dt <- sum(diag(tab_validate))/sum(tab_validate)
print(paste("Misclassification: " , (1-m_dt)*100 , "| Accuracy: ", m_dt*100))

correct =0;
wrong =0;


for (x in 1:nrow(train)) {
  
  n_pos = n_neg =0;
  
  #print( paste("Instance" , x))
  
  r1 = predict(nb.fit, train[x,1:8])
  r2 = predict(tree, train[x,1:8])
  r3 = predict(svmmodel, train[x,1:8])
  dt2 = predict(pfit,train[x,1:8])
  if(dt2[1]>dt2[2]){
    r4 = "neg"
  }else{
    r4 = "pos"
  }
  
  #print(paste("Naive Baayes :", r1, "|  DT:", r2, "|  SVM:",r3))
  
  if( r1 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1 
  }
  
  if( r2 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1
  }
  
  if( r3 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1
  }
  
  if( r4 == "pos"){
    n_pos <- n_pos+1
  }else{
    n_neg <- n_neg +1
  }
  
  
  if(n_pos == n_neg){
    eq <- eq+1
    if(train[x,9] == r3){
      correct <- correct +1
    }else{
      wrong <- wrong+1
    }
  }
  #print(paste("Num Pos: ", n_pos , "Num Neg: ", n_neg))
  else if(n_pos > n_neg){
    #print(paste("Ensemble Output: Positive "))
    #print(" ", quote = FALSE)
    if(train[x,9] == "pos"){
      correct <- correct +1
    }else{
      wrong <- wrong+1
    }
    
  }else{
    #print(paste("Ensemble Output: Negative "))
    #print(" ", quote = FALSE)
    if(train[x,9] == "neg"){
      correct <- correct +1
    }else{
      wrong <- wrong+1
    }
    
  }
  #print(PimaIndiansDiabetes[x,9])
}

print(correct)
print(wrong)

nrow(validate)

print(correct / (correct + wrong))
print(eq)
