rm(list=ls())
library(readxl)
master.dataset = read_excel("HealthInsurance.xlsx")
colnames(master.dataset) = tolower(make.names(colnames(master.dataset)))
attach(master.dataset)
summary(master.dataset)
nrow(master.dataset)
ncol(master.dataset)

#Preprocessing   --------------------------------------------------------------------

is.null(master.dataset)

master.dataset$healthcondition = ifelse(master.dataset$verygood == 1, 'verygood',
                                        ifelse(master.dataset$good == 1, 'good',
                                               ifelse(master.dataset$fair == 1, 'fair',
                                               ifelse(master.dataset$poor == 1, 'poor', NA))))   
                  
sum(is.na(master.dataset$healthcondition))

master.dataset$healthcondition[is.na(master.dataset$healthcondition)] = "notknown"

master.dataset$healthc = as.numeric(as.factor(master.dataset$healthcondition))

levels(master.dataset$healthc)

master.dataset$income = master.dataset$income*1000

# Exploratory Data Analysis----------------------------------------------------------------------------------

hist(master.dataset$medexpense, prob=T, breaks=20, xlim=c(0,15000), col="skyblue")
densityplot = density(master.dataset$medexpense)
lines(densityplot, col="red", pch=50)
hist(master.dataset$income,col="skyblue")
hist(master.dataset$age,col="skyblue")

master.dataset = subset(master.dataset, select = -c(healthcondition))

#correlation matrix for continuous variable
temp.cor.data = master.dataset
library(corrplot)
xx = cor(temp.cor.data)
corrplot(xx,method="circle")


# ------------------------------------------------------------------------------------

set.seed(57148852)
sample.dataset=master.dataset[sample(1:nrow(master.dataset),500,replace=FALSE),]

names(master.dataset)

sample.dataset = subset(sample.dataset, select = -c(lowincome, firmsize, firmlocation, 
                                                    educyr, hisp, black, married, verygood, fair, poor, good, 
                                                    midincome, agesqrd, logincome, vgh, fph))

#------------------------------------------------------------------------------------

medex1 = lm(logmedexpense~healthins+age+female+income+illnesses+ssiratio+
              +private+blackhisp+poverty+msa+prioritylist+healthc,
              data=sample.dataset)

summary(medex1)

medex2 = lm(logmedexpense~healthins+age+female+blackhisp+log(income)+illnesses
              +private+prioritylist+ssiratio+msa+
              I(income^2)+I(illnesses^2)+I(ssiratio^2), data=sample.dataset)

summary(medex2)

medex3 = lm(logmedexpense~healthins+age+female+blackhisp+illnesses+log(income)+
              ssiratio+private+prioritylist+I(illnesses^2)
            +I(ssiratio^2)+log(income):illnesses, data=sample.dataset)

summary(medex3)
confint(medex3)


(exp(coef(medex3)["illnesses"]) - 1)*100 #gives percentage
(exp(coef(medex3)["illnesses"])) #gives exact value

#-----------------------predictions-------------------------------------------------------------------------
newdata=data.frame(healthins=0 ,age=75, female=1, blackhisp=1 ,illnesses= 2, income = 21118,  
                     ssiratio=0, private=0, prioritylist=1)

exp(predict(medex3, newdata, interval="predict"))
predict(medex3, newdata, interval="predict")

#------------------------------------------------------------------------------------------------------------

library(stargazer)
stargazer(medex1,medex2,medex3,type="text",title="Regression Results",single.row=TRUE)

#-------------------------------------------------------------------------------------------------------
#LINE Assumptions
#Linearity 
plot(sample.dataset$logmedexpense,medex3$fitted.values,pch=19,xlim=c(0,10),ylim=c(0,10), main=" Actuals v. Fitted")
abline(0,1,col="red",lwd=3)  

#qqnorm(ols2$res)                              # Q-Q plot
#qqline(ols2$res, col="red")

#Normality
qqnorm(medex3$residuals,pch=19,
       main="Normality Plot")
qqline(medex3$residuals,lwd=3,col="red")

shapiro.test(medex3$res)      
# p value less than 0.05 hence data is not normally distributed    
# Shapiro-Wilk's test of multivariate normality

#Equality of variances
# install.packages("car")

#Equality of Variances
plot(sample.dataset$logmedexpense,medex3$residuals,xlim=c(0,10),ylim=c(-4,4),pch=19,main="Residual Plot")
abline(0,0,col="red",lwd=3)

library("car")
bartlett.test(list(medex3$res, medex3$fit))       # Bartlett's test of homoskedasticity
#reject null hypothesis unequal variances.  


leveneTest(medex3$res, medex3$fit, center=mean)   # Levene's test of homoskedasticity

# install.packages("lmtest")
library(lmtest)
dwtest(medex3)                                    # Durbin-Watson test of autocorrelation

#close to 2 no autocorrelation

#Multicollinerity
car::vif(medex3)


