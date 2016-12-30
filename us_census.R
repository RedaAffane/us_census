library(VIM)
library(plyr)
library(glmnet)
library(leaps)
library(boot)
library(FactoMineR)


census = read.csv("./census_income_learn.csv",header=FALSE)
colnames(census) = c("age",
                     "class of worker",
                     "detailed industry recode",
                     "detailed occupation recode",
                     "education",
                     "wage per hour",
                     "enroll in edu inst last wk",
                     "marital stat",
                     "major industry code",
                     "major occupation code",
                     "race",
                     "hispanic origin",
                     "sex",
                     "member of a labor union",
                     "reason for unemployment",
                     "full or part time employment stat",
                     "capital gains",
                     "capital losses",
                     "dividends from stocks",
                     "tax filer stat",
                     "region of previous residence",
                     "state of previous residence",
                     "detailed household and family stat",
                     "detailed household summary in household",
                     "instance weigth",
                     "migration code-change in msa",
                     "migration code-change in reg",
                     "migration code-move within reg",
                     "live in this house 1 year ago",
                     "migration prev res in sunbelt",
                     "num persons worked for employer",
                     "family members under 18",
                     "country of birth father",
                     "country of birth mother",
                     "country of birth self",
                     "citizenship",
                     "own business or self employed",
                     "fill inc questionnaire for veteran's admin",
                     "veterans benefits",
                     "weeks worked in year",
                     "year",
                     "income")

### Some values should be converted to categorical varaibles
census$year = as.factor(census$year)
census$`veterans benefits` = as.factor(census$`veterans benefits`)
census$`own business or self employed` = as.factor(census$`own business or self employed`)
census$`detailed industry recode` = as.factor(census$`detailed industry recode`)
census$`detailed occupation recode` = as.factor(census$`detailed occupation recode`)




### Missing values ###
# We will replace the missing values in the variables "country of birth father",
#"country of birth mother" and "country of birth self" by the value " United-States" which the most
#frequent value

census$`country of birth father`[census$`country of birth father` == " ?"] <- " United-States"
census$`country of birth mother`[census$`country of birth mother` == " ?"] <- " United-States"
census$`country of birth self`[census$`country of birth self` == " ?"] <- " United-States"
census$`state of previous residence`[census$`state of previous residence` == " ?"] <- " Not in universe"

#aggr_plot <- aggr(census, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
#                  labels=names(census), cex.axis=.7, gap=3, 
#                  ylab=c("Histogram of missing data","Pattern"))

### Age ###
#Let's create a new variable age_range that contains 3 levels
census$age_range=as.factor(as.numeric(lapply(census$age, function(x){
  if(x<=20){
    return(0)
  }
  else if(x>=62){
    return(2)
  }
  else{
    return(1)
  }
})))

#revalue(census$age_range, c("0":"below 20","1":"between 21 and 61","2":"above 62"))
levels(census$age_range)[levels(census$age_range)=="0"] <- "below 20"
levels(census$age_range)[levels(census$age_range)=="1"] <- "between 21 and 61"
levels(census$age_range)[levels(census$age_range)=="2"] <- "above 62"


#Split the dataset set into a training set and a validation set
train_indexes = sample(seq_len(nrow(census)), size=floor(0.75*nrow(census)) )
train = census[train_indexes,]
test = census[-train_indexes,]


#### Logistic regression
logistic_regression = glm(income~.,data=train,family = binomial)
summary(logistic_regression)
predicted_proba = predict(logistic_regression,newdata = test, type = "response")
predicted_values = ifelse(predicted_vales >0.5,2,1)
table(predicted_values,as.numeric(test$income))
mean(predicted_values == as.numeric(test$income))
#####

### srtepwise selection
stepwise_reg = regsubsets(income~.,data=census,nvmax=40,method="forward")
stepwise_sum = summary(stepwise_reg)
plot(stepwise_sum$cp,xlab="Number of Variables",ylab="Mallow's Cp")
#####


###Penalized models
x=model.matrix(income~.,data=train) 
y=train$income
### Ridghe regression
ridge_cross = cv.glmnet(x,y,family="binomial",alpha=0)
plot(ridge_cross)
lambda = ridge_cross$lambda.1se
ridge_coef = data.matrix(coef(ridge_cross))

fit.ridge=glmnet(x,y,family="binomial",alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
abline(v=log(lambda),pch=19,col="blue")

ridge_prob=predict.cv.glmnet(ridge_cross,newx=model.matrix(income~.,data=test),type="response") 
ridge_prediction=ifelse(ridge_prob >0.5,2,1)
table(ridge_prediction,as.numeric(test$income))
mean(ridge_prediction == as.numeric(test$income))

#plot the coefficients
ridge_coef = ridge_coef[-1:-2,]
barplot(sort(ridge_coef),horiz=TRUE,cex.names=0.5,ylab="Features",xlab="Features coefficient",yaxt='n')
###########

### Lasso
lasso = glmnet(x,y,family="binomial",alpha=1)
plot(lasso,xvar="lambda",label=TRUE)
abline(v=log(lambda),pch=19,col="blue")


lasso_cross_validated = cv.glmnet(x,y,family="binomial",alpha=1)
plot(lasso_cross_validated)
lambda = lasso_cross_validated$lambda.1se
lasso_coef = data.matrix(coef(lasso_cross_validated))


lasso_prob=predict.cv.glmnet(lasso_cross_validated,newx=model.matrix(income~.,data=test),type="response") 
lasso_prediction=ifelse(lasso_prob >0.5,2,1)
table(lasso_prediction,as.numeric(test$income))
mean(lasso_prediction == as.numeric(test$income))

#plot the coefficients
lasso_coef = lasso_coef[-1:-2,]
barplot(sort(lasso_coef),horiz=TRUE,cex.names=0.5,ylab="Features",xlab="Features coefficient",yaxt='n')

#####
