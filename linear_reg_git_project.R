# Reading data
#setwd("C:/Users/sherr/Desktop/Columbia/Linear Rgression Model/Project/cancer_reg.csv")
df = read.csv("C:/Users/sherr/Desktop/Columbia/Linear Rgression Model/Project/cancer_reg.csv")
head(df)
ncol(df)

df1=df
# Dropping variables
# Add binnedInc,Geography later
df = subset(df, select = -c(binnedInc,Geography,PctSomeCol18_24,PctPrivateCoverageAlone) ) # see explanation below(the next one)
head(df)
ncol(df)

# Why did we remove pctsomecol18_24
miss=colSums(is.na(df1))    
miss=data.frame(miss)# Count missing values by column
# x1 x2 x3 
miss=miss%>%arrange(desc(miss))
miss <- cbind(newColName = rownames(miss), miss)
rownames(miss) <- 1:nrow(miss)
miss=miss%>%filter(miss>0)%>%arrange(desc(miss))
library(ggplot2)
ggplot(miss,aes(x=newColName ,y=miss))+geom_bar(stat="identity")+labs(x="Columns with missing value",y="Missing values",title="Missing value")
# As 80% of values are missing in pctsomecol18_24 we remove that.Pct provate coverage alone has missing values as well it gets out of vif at end . hence we take it out in this stage


# Filling missing values
# Did this by median
apply(is.na(df),2,sum)
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] = median(df[,i], na.rm = TRUE)
}
apply(is.na(df),2,sum)

# Fixing incorrect values in Age by Median
boxplot(df$MedianAge)
df$MedianAge = replace(df$MedianAge , df$MedianAge>150 , median(df$MedianAge))
boxplot(df$MedianAge)

#CHECKING NORMALITY before making changes
model3=lm(TARGET_deathRate~., data = df)
shapiro.test(model3$residuals)
## Hence we see residuals are very bad and normality is not there , Normality here has p-value < 2.2e-16
qqnorm(model3$residuals, pch = 1, frame = FALSE)
qqline(model3$residuals, col = "steelblue", lwd = 2)


# Making the response "TARGET_deathRate" normal
# Removing outliers based on interquartile
S = df
hist(S$TARGET_deathRate)
S = S[!S$TARGET_deathRate %in% boxplot.stats(S$TARGET_deathRate)$out,]
nrow(S)
hist(S$TARGET_deathRate)


## Removing predictors with high correlation using VIF
corrplot::corrplot(cor(S),tl.cex=0.7)
# VIF
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
cor(S$TARGET_deathRate,S$MedianAgeMale)
cor(S$TARGET_deathRate,S$MedianAgeFemale)
cor(S$TARGET_deathRate,S$MedianAge)
S = subset(S, select = -c(MedianAge) )
# Removed MedianAge

# VIF
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
cor(S$TARGET_deathRate,S$MedianAgeMale)
cor(S$TARGET_deathRate,S$MedianAgeFemale)
cor(S$MedianAgeMale,S$MedianAgeFemale)
corrplot::corrplot(cor(S),tl.cex=0.7)
S = subset(S, select = -c(MedianAgeMale) )
# Removed MedianAgeMale

# VIF
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
cor(S$TARGET_deathRate,S$avgAnnCount)
cor(S$TARGET_deathRate,S$avgDeathsPerYear)
cor(S$TARGET_deathRate,S$popEst2015)
S = subset(S, select = -c(avgDeathsPerYear) )
# Removed avgDeathsPerYear

# VIFs
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
corrplot::corrplot(cor(S),tl.cex=0.7)
cor(S$TARGET_deathRate,S$PctPrivateCoverage)
cor(S$TARGET_deathRate,S$PctPublicCoverage)
cor(S$TARGET_deathRate,S$PctPublicCoverageAlone)
cor(S$TARGET_deathRate,S$PctEmpPrivCoverage)
S = subset(S, select = -c(PctEmpPrivCoverage) )
# Removed PctEmpPrivCoverage

# VIFs
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
cor(S$TARGET_deathRate,S$PctPrivateCoverage)
cor(S$TARGET_deathRate,S$PctPublicCoverage)
cor(S$TARGET_deathRate,S$PctPublicCoverageAlone)
S = subset(S, select = -c(PctPrivateCoverage) )
# Removed PctPrivateCoverage

# VIFs
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
cor(S$TARGET_deathRate,S$PctPublicCoverage)
cor(S$TARGET_deathRate,S$PctPublicCoverageAlone)
S = subset(S, select = -c(PctPublicCoverage) )
# Removed PctPublicCoverage

# All variables have below 10 VIF
model = lm(TARGET_deathRate~., data = S)
car::vif(model)
corrplot::corrplot(cor(S), tl.cex=0.7)


# Include all variables for now, check later if you can reduce the number of predictors
# 22 predictors + 1 response
names(S)
model = lm(TARGET_deathRate~., data = S)
summary(model)


# Stepwise Regression
# I have not used the shortlisted predictors from stepwise in model 8
# I used the terms from stepwise in model 9
model = lm(TARGET_deathRate~., data = S)
library(olsrr)
k = ols_step_both_p(model, progress = T)        # This is forward stepwise...backward stepwise would be more ideal
k


# Using variables shortlisted from VIF and stepwise:
setwd("C:/Users/Kartik Kotian/Desktop/Uni/GR5205 - Linear Regression Models/Project/Cancer OLS")
df = read.csv("cancer_reg.csv")
S = subset(df, select = c(TARGET_deathRate,PctBachDeg25_Over,incidenceRate,PctOtherRace,PctHS18_24,PctMarriedHouseholds,PctUnemployed16_Over,PctHS25_Over,MedianAgeFemale,BirthRate,PercentMarried,PctEmployed16_Over,PctWhite,PctPublicCoverageAlone,avgAnnCount,popEst2015,povertyPercent) )
head(S) # consisting of all variables from stepwise 

# Missing value fix
apply(is.na(S),2,sum)
for(i in 1:ncol(S)){
  S[is.na(S[,i]), i] = median(S[,i], na.rm = TRUE)
}
apply(is.na(S),2,sum)


# Make Y normal
shapiro.test(S$TARGET_deathRate)     # Far from normal
hist(S$TARGET_deathRate)
S = S[!S$TARGET_deathRate %in% boxplot.stats(S$TARGET_deathRate)$out,]
nrow(S)
hist(S$TARGET_deathRate)
shapiro.test(S$TARGET_deathRate)     # Much better


# Y outlier
Y = S[,c(1)]                       # Separating response column
df1 = S[,-c(1)]                    # All X value DF
DF = cbind(Y,df1)                   # Entire DF with Y as first variable followed by all X variables
X = cbind(rep(1,nrow(S)),df1)      # 1 column followed by all X variables
X = as.matrix(sapply(X, as.numeric))
n = nrow(DF)
p = 16
model = lm(Y~., data=DF)
e = Y - predict(model)
length(Y)
length(predict(model))
SSE = sum(e^2)
hii = rep(0,nrow(DF))
for (i in 1:nrow(DF)){
  Xit = X[i,]                  # Xi'
  Xi = t(t(Xit))               # Xi
  XtXI = solve(t(X)%*%X)       # Inverse of X'X
  hii[i] = Xit%*%XtXI%*%Xi
}
head(hii)
Num = n-p-1
Den = SSE*(1-hii)-e^2
ti = e*(Num/Den)^0.5
ti                         # Studentized deleted residuals vector
tcalc = abs(ti[which.max(abs(ti))])     # Max absolute value of ti
tcalc
tcrit = qt(1-(0.1/(2*n)),n-p-1)         # t Critical value at alpha 0.1
tcrit
sum(abs(ti)>4.159259)  
# There are 7 values that show up above the crit value.
ti[abs(ti)>4.159259]
nrow(DF)
DF = DF[-c(282,1059,2646,2659)]     # New dataframe after removing Y outliers
nrow(DF)


# Checking Y normality
library(MASS)
bc <- boxcox(Y~1, data = DF,lambda = seq(-2, 2, 1/10))
pow=bc$x[which.max(bc$y)]
# 0.9
DF$Y = (DF$Y)^pow
hist(DF$Y)
shapiro.test(DF$Y)
## Hence we transformed normality from 2*1

# Are residuals normal?
model = lm(Y~., data=DF)
e = Y - predict(model)
shapiro.test(e)
# P value decreased from  pvalue< 2.2e-16 to p=0.0003

# LINEARITY 
plot(model,1)
# We can see linearity

# CHECKING CONSTANT VARIANCE
xxxxxxxx
# Weighted regression didnt work
# Weigted regression
wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2

#perform weighted least squares regression
wls_model <- lm(Y ~ ., data = DF, weights=wt)
bptest(wls_model)
plot(wls_model, 1)


# Fitting model
model = lm(Y~., data = DF)
summary(model)
# here we see Multiple R-squared:  0.4716,	Adjusted R-squared:  0.4687 

# CV score
library(caret)
set.seed(0)
train.control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
Model = train(Y~., data = DF, method = "lm", trControl = train.control)
print(Model)


xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Lasso
y = Y

#define matrix of predictor variables

x = subset(DF, select = -c(Y))
x = as.matrix(x)

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
?cv.glmnet
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.06431338

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# R^2 ......0.4717

# Resdiual
e = y - y_predicted

# Are residuals normal?
shapiro.test(e)
# No

# Variance constant?
plot(y_predicted,e)
# No

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Ridge
y = Y

#define matrix of predictor variables
x  
x = subset(DF, select = -c(Y))
x = as.matrix(x)

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
?cv.glmnet
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 1.17741

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
?glmnet

y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# R^2 ......0.4717

# Resdiual
e = y - y_predicted

# Are residuals normal?
shapiro.test(e)
# No

# Variance constant?
plot(y_predicted,e)
# No


xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Fixing other assumptions in the initial multiple regression: (Ongoing)

# Linearity of data:
# Fitted vs Residual plot should not have pattern
plot(model, 1)
# Not ideal, but seems good enough


# #   Trying to relation of each variable in the model linear with X transformations:
# names(DF)
# plot(DF)

#  S$PctOtherRace
#  names(S)
#  plot(DF$PctOtherRace,DF$Y)
#  cor(DF$PctOtherRace,DF$Y)
#  hist(DF$PctOtherRace)
#  plot(log(DF$PctOtherRace+1),DF$Y)
#  cor(log(DF$PctOtherRace+1),DF$Y)
#  hist(log(DF$PctOtherRace+1))
# 
#  povertyPercent
#  names(S)
#  plot(DF$povertyPercent,DF$Y)
# cor(DF$povertyPercent,DF$Y)
#  hist(DF$povertyPercent)
#  plot(log10(DF$povertyPercent),DF$Y)
#  cor(log10(DF$povertyPercent),DF$Y)
# hist(log10(DF$povertyPercent),breaks=50)
# 
# 
# plot(DF$avgAnnCount,DF$Y)
#  cor(DF$povertyPercent,DF$Y)
# plot(log10(DF$avgAnnCount),DF$Y)
# cor(log10(DF$avgAnnCount),DF$Y)
# 
# S$PctOtherRace = log(S$PctOtherRace+1)
# S$povertyPercent = log10(S$povertyPercent)


# Is variance of error terms constant?
library(lmtest)
bptest(model)
# No





# Check which predictors can be added as power and intercation terms:
plot(e,DF$PctBachDeg25_Over)

mod <- lm(Y ~ .+I(PctBachDeg25_Over^2),data = DF)
summary(mod)

e = Y - predict(mod)
plot(e,DF$PctBachDeg25_Over)
plot(mod, 1)
# Error terms are better distributed...check for all power terms and interactions
# Ongoing


# CV score
library(caret)
set.seed(0)
train.control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
Model = train(Y~., data = DF, method = "lm", trControl = train.control)
print(Model)


