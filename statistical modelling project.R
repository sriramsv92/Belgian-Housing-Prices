rm(list=ls())
studentnumber=0439234
fulldata = read.csv(file=file.choose(), sep = " ", header = TRUE)
digitsum = function(x) sum(floor(x/10^(0:(nchar(x)-1)))%%10)
set.seed(studentnumber)
mysum = digitsum(studentnumber)
if((mysum %% 2) == 0) { # number is even
  rownumbers = sample(1:327,150,replace=F)
} else { # number is odd
  rownumbers = sample(309:585,150,replace=F)
}
mydata = fulldata[rownumbers,]

#(i)
library(lattice)
library(lme4)

mydata2<-mydata[,-3]
library(mgcv)
library(SemiPar)
attach(mydata2)
x1 = as.factor(Region)
x2 = as.factor(Province)
x3 = Shops
x4 = Bankruptcies
x5 = MeanIncome
x6 = TaxForms
x7 = HotelRestaurant
x8 = Industries
x9 = HealthSocial
y = PriceHouse


par(mfrow=c(3,3))
#checking for the variables for which smoothing is required
scatter.smooth(x3, y)
scatter.smooth(x4, y)
scatter.smooth(x5, y)
scatter.smooth(x6, y)
scatter.smooth(x7, y)
scatter.smooth(x8, y)
scatter.smooth(x9, y)
par(mfrow=c(1,3))
#model fitting
model1 = gam(y~ x1 + x2 + s(x3) + s(x4) + s(x5,bs='cr') + s(x6) + s(x7) + (x8) + (x9),  omit.missing = TRUE,family=gaussian(link=log) , method = "REML")
model2 = gam(y~ x1 + x2 + s(x3,bs='cr') + s(x4,bs='cr') + s(x5,bs='cr') + s(x6,bs='cr') + s(x7,bs='cr') + (x8) + (x9),  omit.missing = TRUE,family=gaussian(link=log) , method = "REML")
model3 = gam(y~ x1 + x2 + s(x5,bs='cr') + s(x6,bs='cr') + s(x7,bs='cr') + (x8) + (x9),  omit.missing = TRUE,family=gaussian(link=log),  method = "REML")
model4 = gam(y~ x1 + x2 + s(x5,bs='cr') + s(x6,bs='cr') + s(x7,bs='cr')+(x9),  omit.missing = TRUE,family=gaussian(link=log),  method = "REML")
model11 = gam(y~ x1 + x2  + s(x4) + s(x5) + s(x6) + s(x7) + (x8) + (x9),  omit.missing = TRUE,family=gaussian(link=log) , method = "REML")
model12 = gam(y~ x1 + x2  + s(x6) + s(x7) + s(x5) + (x9),  omit.missing = TRUE,family=gaussian(link=log) , method = "REML")
summary(model4)
scatter.smooth(x6,y)
scatter.smooth(x7,y)
scatter.smooth(x5,y)

par(mfrow=c(1,3))
#partial coefficients plot
plot(model4, all.terms = TRUE, residuals = FALSE, pch=1,cex=1, shade=TRUE,shade.col='gray80')

par(mfrow=c(2,2))
gam.check(model4)

#2. Define a polynomial function

Polynomial = function(x1,x2,degree) {
  data=rep(NA,length(x1))
  p.x1=cbind(1,poly(x1,degree))
  #creates matrix with #degree+1 columns, first one is intercept other #degree are for polynomials
  p.x2=cbind(1,poly(x2,degree))
  for(j in 1:degree){
    for(i in 0:j){
      
      
      data=cbind(data,p.x1[,j-i+1]*p.x2[,i+1])
      colnames(data)[ncol(data)]<-paste("X",j-i,".",i,sep="")
      #Xa.b is interaction between a'th polynomial of x1 and b'th polynomial of x2
      #X1.0 is thus just the first polynomial of x1 since 0 denotes the intercept
    }
  }
  #delete column with NA's from data
  data=data[,-1]
  return(data)
}
#maximum degree of 5
expandata <- data.frame(Polynomial(mydata$TaxForms,mydata$HealthSocial, degree = 5))
names(expandata)
#add column with median house price
Cn<-4.18 #for 95% confidence
expandata <- data.frame(cbind(PriceHouse=mydata$PriceHouse,expandata))
nullmodel<-lm(PriceHouse~X1.0+X0.1+X1.1+X2.0+X0.2,data=expandata)
summary(nullmodel)


AIC(nullmodel)
logLik(nullmodel)
loglik_null <- logLik(nullmodel)

dim(expandata)
M <- dim(expandata)[2]

lowest=10000000
t<-aic<-numeric(M-5)
for(i in 5:M){
  expandata_sub<-expandata[1:i]
  
  nest<-lm(PriceHouse~.,data=expandata_sub)
  loglik_nest<-logLik(nest)
  LRT_nest<-Cn*(loglik_nest-loglik_null)
  #Cn=4.18 to test at signif 5%
  t[i-5]<-LRT_nest/(i-5)
  aic[i-5]<-AIC(nest)
  if(AIC(nest)<lowest)
  {
   lowest=AIC(nest)
  model=expandata_sub
  b=nest
  }
}
#selected model
T_nos<-max(t)
T_nos
#9.0728
m<-100

pvalue<-1-exp(-sum((1-pchisq((1:m)*T_nos,1:m))/(1:m)))
pvalue
# p-value is 0.0026. hence we reject the null hypothesis and say that there is a better model compared to the null
#3 trellis plot for Tax Forms
xyplot(y ~ x6|x2, type= c("g", "p", "r"),
       xlab = "x6: The Number of Tax Forms",ylab='Median Price of a House',drop.unused.levels = TRUE)
#(ii)

# trellis plot for other variables

xyplot(y ~ x3|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)
xyplot(y ~ x4|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)
xyplot(y ~ x5|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)
xyplot(y ~ x7|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)
xyplot(y ~ x8|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)
xyplot(y ~ x9|x2, type= c("g", "p", "r"),
       drop.unused.levels = TRUE)

library(hglm)
library(lme4)
# random effects model
mixed_model = lmer( y ~ x3 + x4 + x5 + x6 + x7 + x8 + x9 + (1+x3 + x4 + x5 + x6 + x7 + x8 + x9|x2), method="REML")
summary(mixed_model)
(coef(mixed_model))
plot(mixed_model)
#including only tax forms
mixed_model2=lmer(y ~ x3 + x4 + x5 + x6 + x7 + x8 + x9 + ( 1+x6 |x2), method="REML")
summary(mixed_model2)
(coef(mixed_model2))
plot(mixed_model2)


#4


library(tidyverse)

#(i) Lens is the chosen municipality based on the given criteria

y2 = mydata[c("PriceHouse")]
y2 = y2[,1]

install.packages("devtools")
library(devtools)
install_github("chjackson/fic",force=TRUE)
library(fic)
X = mydata[c( "Province","Shops", "Bankruptcies", "MeanIncome",
              "TaxForms", "HotelRestaurant", "Industries", "HealthSocial")]

focus_median = function(par, X) {X %*% par}

model_allvariables = glm(y ~  ., family = gaussian(link=log), data = X)

inds0 = c(1,rep(0,12))#13 parameters in wide model


combs = all_inds(model_allvariables, inds0)

selected_matrix = model.matrix(model_allvariables)


ficres = fic(wide=model_allvariables, inds=combs, inds0=inds0, focus=focus_median, X=selected_matrix)



#Lens index
which(mydata["Municipality"]=="Lens")[1]


summary(ficres)$min[15,]
#selected model:
model_lens = glm(y2 ~ Province + MeanIncome + Industries, family=gaussian(link=log))
summary(model_lens)
#the predicted value is exp(4.971189)

(ii)#Moeskroen
which(mydata["Municipality"]=="Moeskroen")[1]
#95
summary(ficres)$min[95,]
#selected model:
model_moeskroen = glm(y2 ~ Province + Bankruptcies + MeanIncome + 
                        HotelRestaurant + HealthSocial, family = gaussian(link=log))
summary(model_moeskroen)

# the FIC is 4.813264


#5


library(dplyr)
xmatrix <- mydata2 %>% select(Region, Province, Shops,Bankruptcies,MeanIncome,TaxForms,HotelRestaurant,Industries,HealthSocial) %>% data.matrix()
design_matrix = lm(y ~  factor(Region) + factor(Province) + Shops + Bankruptcies + MeanIncome + TaxForms + HotelRestaurant +
                     Industries + HealthSocial)
xmatrix = model.matrix(design_matrix)

#using h20

library(h2o)
h2o.init()
mydata2$Province=factor(mydata2$Province)
mydata2$Region=factor(mydata2$Region)
mydat2=as.h2o(mydata2)
mydat2$Province=as.factor(mydat2$Province)
mydat2$Region=as.factor(mydat2$Region)
mydat3 = as.h2o(xmatrix)

X = c(x1, x2, x3, x4, x5, x6, x7, x8, x9)
fit_mle = glm(PriceHouse~.,family=gaussian(link=log),data=mydata2)
round(coef(fit_mle),5)
ridge2=h2o.glm(y='PriceHouse',x=c('Region','Province','Shops','Bankruptcies','MeanIncome','TaxForms','HotelRestaurant','Industries','HealthSocial'),training_frame = mydat2,family='gaussian',link='log',nfolds=0,alpha=0,lambda_search=TRUE)


lasso2=h2o.glm(y='PriceHouse',x=c('Region','Province','Shops','Bankruptcies','MeanIncome','TaxForms','HotelRestaurant','Industries','HealthSocial'),training_frame = mydat2,family='gaussian',link='log',nfolds=0,alpha=1,lambda_search=TRUE)
elasticnet2=h2o.glm(y='PriceHouse',x=c('Region','Province','Shops','Bankruptcies','MeanIncome','TaxForms','HotelRestaurant','Industries','HealthSocial'),training_frame = mydat2,family='gaussian',link='log',nfolds=0,alpha=0.5,lambda_search=TRUE)
test.Lens<-subset(mydata,Municipality=='Lens')
test.Moeskroen<-subset(mydata,Municipality=='Moeskroen')
test.Lens=test.Lens
test.Lens2=test.Lens
test.Moeskroen2=test.Moeskroen
test.pred.Lens=exp(predict(fit_mle,test.Lens2))
test.pred.Moeskroen=exp(predict(fit_mle,test.Moeskroen2))

test.Lens=as.h2o((test.Lens))
test.Moeskroen<-as.h2o(test.Moeskroen)
pred.ridge.Lens=h2o.predict(ridge2,test.Lens)
pred.ridge.Moeskroen=h2o.predict(ridge2,test.Moeskroen)
pred.Lasso.Lens=h2o.predict(lasso2,test.Lens)
pred.Lasso.Moeskroen=h2o.predict(lasso2,test.Moeskroen)
pred.Elasticnet.Lens=h2o.predict(elasticnet2,test.Lens)
pred.Elasticnet.Moeskroen=h2o.predict(elasticnet2,test.Moeskroen)
Lens=141.250
Moeskroen=135

#using glmnet

library(glmnet)
lambdas = 10^seq(10, -2, by=-.1)

glmnet_fit_ridge = cv.glmnet(xmatrix, log(y), alpha = 0, lambda = lambdas,family='gaussian')

plot(glmnet_fit_ridge)
opt_lambda_ridge = glmnet_fit_ridge$lambda.min
ridge_fit = glmnet_fit_ridge$glmnet.fit
predictions_ridge_glmnet = predict(ridge_fit, s = opt_lambda_ridge,newx=xmatrix)
Lens_Moeskroen=c(381,389,15,95)
exp(predictions_ridge_glmnet[15])
y[15]
exp(predictions_ridge_glmnet[95])
y[95]

round(coef(glmnet_fit_ridge, s = c(opt_lambda_ridge))[,1],6)

glmnet_fit_lasso = cv.glmnet(xmatrix, log(y), alpha = 1, lambda = lambdas)

plot(glmnet_fit_lasso)
opt_lambda_lasso = glmnet_fit_lasso$lambda.min
lasso_fit = glmnet_fit_lasso$glmnet.fit
predictions_lasso_glmnet = predict(lasso_fit, s = opt_lambda_lasso,newx=xmatrix)

exp(predictions_lasso_glmnet[15])
y[15]
exp(predictions_lasso_glmnet[95])
y[95]

round(coef(glmnet_fit_lasso, s = c(opt_lambda_lasso))[,1],6)

glmnet_fit_elastic = cv.glmnet(xmatrix, log(y), alpha = 0.5, lambda = lambdas)

plot(glmnet_fit_elastic)
opt_lambda_elastic = glmnet_fit_elastic$lambda.min
elastic_fit = glmnet_fit_elastic$glmnet.fit
predictions_elastic_glmnet = predict(elastic_fit, s = opt_lambda_elastic,newx=xmatrix)

exp(predictions_elastic_glmnet[15])
y[15]
exp(predictions_elastic_glmnet[95])
y[95]

round(coef(glmnet_fit_elastic, s = c(opt_lambda_elastic))[,1],6)
