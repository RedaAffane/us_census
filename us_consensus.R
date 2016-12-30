library(VIM)
library(plyr)
library(glmnet)
library(leaps)
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
str(census)
attach(census)
### Missing values ###
# We will replace the missing values in the variables "country of birth father",
#"country of birth mother" and "country of birth self" by the value " United-States" which the most
#frequent value

census$`country of birth father`[census$`country of birth father` == " ?"] <- " United-States"
census$`country of birth mother`[census$`country of birth mother` == " ?"] <- " United-States"
census$`country of birth self`[census$`country of birth self` == " ?"] <- " United-States"

census$`state of previous residence`[census$`state of previous residence` == " ?"] <- " Not in universe"

log_reg = glm(income~.,data=census,family = binomial)

stepwise_reg = regsubsets(income~.,data=census,nvmax=40,method="forward")
stepwise_sum = summary(stepwise_reg)
plot(stepwise_sum$cp,xlab="Number of Variables",ylab="Mallow's Cp")

x=model.matrix(income~.,data=census) 
y=census$income

ridge_cross = cv.glmnet(x,y,family="binomial",alpha=0)
plot(ridge_cross)
lambda = ridge_cross$lambda.1se
ridge_coef = data.matrix(coef(ridge_cross))

fit.ridge=glmnet(x,y,family="binomial",alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
abline(v=log(lambda),pch=19,col="blue")



data.frame(levels = unique(census$income), value = as.numeric(unique(census$income)))


aggr_plot <- aggr(census, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(census), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

logistic_regression = glm(income~.,data=census,family=binomial)
