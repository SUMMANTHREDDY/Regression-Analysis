#####################################################################
#                  Data Used : Red White Wine Quality                     #
#####################################################################
library(caret)
options(rgl.useNULL = TRUE)
library(rgl)
library (ggplot2)
library(qpcR) #has PRESS residuals
library(rgl)
library(robustbase)
library(MPV)

df <- read.csv("wine-quality-white-and-red 2.csv")
df

print(is.data.frame(df))
cat('The number of columns in the data is:',ncol(df),'\n')
cat('The number of rows in the data is:',nrow(df))

y = df$quality
x1 = df$fixed.acidity
x2 = df$volatile.acidity
x3 = df$citric.acid
x4 = df$residual.sugar
x5 = df$chlorides
x6 = df$free.sulfur.dioxide
x7 = df$total.sulfur.dioxide
x8 = df$density
x9 = df$pH
x10 = df$sulphates
x11 = df$alcohol
x12 = df$type
df

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(df$quality, p = .8,
                                  list = FALSE,
                                  times = 1)
wine <- df[ trainIndex,]
wine
Valid <- df[-trainIndex,]
nrow(wine)
nrow(Valid)


## visulaizing the data with respective to color and quality
library(ggpairs)
qplot(quality, data = wine, fill = type, binwidth = 1) +
  scale_x_continuous(breaks = seq(3,10,1), lim = c(3,10)) +
  scale_y_sqrt()
##visualization
#install.packages("GGally")







res = lm(y~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data=wine)
summary(res)
anova(res)

press = PRESS(res)
SST = sum(anova(res)['Sum Sq'])
R.pred1 = 1 - (press/SST)
R.pred1
# The value of R^2 prediction is 0.2890, which means that we can expect
# our model to explain about 34.83% of the variability in predicting new
# observations.


########################## Adequacy Test ##################################

# Further we can plot the model diagnostic checking for other problems such 
# as normality of error term etc
Stud = rstudent(res)
qqnorm(Stud)
qqline(Stud, col = "steelblue", lwd = 2)
plot(res$fitted.values, Stud)
plot(res)           #will give the clear illustration
# From the plots, data seems to have constant variance
# The qq plot shows a slight deviation from normality.
# We will try boxcox transformation to fix that.

# Small departures from the normality assumption do not affect the model greatly.

library(MASS)
bs <- boxcox(res)
# We see from the graph that the value of lambda is 1, which corresponds to
# a transformation of y-1, which is nothing but a translation, and won't
# make any difference. So we don't go for transformation.

# Checking for outliers
# we don't see any outliers in the residual plots. So we also check for
# outliers using Cook's distance.
D1=cooks.distance(res)
which(D1 >1) 

#no outliers
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

## checking for X1 variable
dfq1<- mutate(df ,bin=ntile(x1,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=df$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

## checking for X2 variable
dfq1<- mutate(wine ,bin=ntile(x2,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))



## checking for X3 variable
dfq1<- mutate(df ,citric=ntile(citric.acid,10))
crop=ggplot(data=dfq1, mapping=aes(x=citric, y=df$quality,group = citric)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))



## checking for X4 variable
dfq1<- mutate(wine ,bin=ntile(x4,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

## checking for X5 variable
dfq1<- mutate(wine ,bin=ntile(x5,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X6 variable
dfq1<- mutate(wine ,bin=ntile(x6,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X7 variable
dfq1<- mutate(wine ,bin=ntile(x7,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X8 variable
dfq1<- mutate(wine ,bin=ntile(x8,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X9 variable
dfq1<- mutate(wine ,bin=ntile(x9,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X10 variable
dfq1<- mutate(wine ,bin=ntile(x10,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop

## checking for X11 variable
dfq1<- mutate(wine ,bin=ntile(x11 , 10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

crop
#
res1 = lm(y~ x1+x2+(x3)^2+x4+x5+x6+x7+x8+x9+x10+x11)
summary(res1)
anova(res1)
plot(res1)








############################# Multicollinearity ##########################
data = wine[,2:12]
library(corrplot)
correlations = cor(data)
corrplot(correlations,method = "number")


res = cor(data)
res
solve(cor(data))

VIF=diag(solve(cor(wine)))
VIF



############################ polynomial ############################

#check for the quadratic
x=wine[,2:12]
colnames(x)=c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11')
quad_x = cbind(x^2)
colnames(quad_x)=c('x1_sq','x2_sq','x3_sq','x4_sq','x5_sq','x6_sq','x7_sq',
                   'x8_sq','x9_sq','x10_sq','x11_sq')
quad_x =cbind(x,quad_x)
# Let's check the correlation 

cor(quad_x)
# another way to check the multicollinearity is VIF
VIF=diag(solve(cor(quad_x)))
VIF

VIF=diag(solve(cor(x)))
VIF

library(corrplot)
correlations = cor(quad_x)
corrplot(correlations)
corrplot

# high correlation
# The correlation matrix shows that the correlation among all the 
# explanatory variables is not very high though there are some values which
# are near to 1. 
# center the data and check again.


cx=sweep(wine[,2:12],2, FUN='-',apply(wine[,2:12],2,mean))
colnames(cx)=c('cx1','cx2','cx3','cx4','cx5','cx6','cx7','cx8',
               'cx9','cx10','cx11')
quad_cx=cbind(cx^2)
colnames(quad_cx)=c('cx1_sq','cx2_sq','cx3_sq','cx4_sq','cx5_sq','cx6_sq','cx7_sq',
                    'cx8_sq','cx9_sq','cx10_sq','cx11_sq')
quad_cx =cbind(cx,quad_cx)   #matrix of cx and cx^2

cor(quad_cx)   
# another way to check the multicollinearity is VIF
VIF=diag(solve(cor(quad_cx)))
VIF

correlations = cor(quad_cx)
corrplot(correlations)
corrplot

# after centering the data correlation  decreased, so let's fit the new model 
cx1 = x1 - mean(x1)
cx2 = x2 - mean(x2)
cx3 = x3 - mean(x3)
cx4 = x4 - mean(x4)
cx5 = x5 - mean(x5)
cx6 = x6 - mean(x6)
cx7 = x7 - mean(x7)
cx8 = x8 - mean(x8)
cx9 = x9 - mean(x9)
cx10 = x10 - mean(x10)
cx11 = x11 - mean(x11)
cx1_sq = cx1^2
cx2_sq = cx2^2
cx3_sq = cx3^2
cx4_sq = cx4^2
cx5_sq = cx5^2
cx6_sq = cx6^2
cx7_sq = cx7^2
cx8_sq = cx8^2
cx9_sq = cx9^2
cx10_sq = cx10^2
cx11_sq = cx11^2


regLC = lm(y~cx1 + cx2 + cx3 + cx4 + cx5 + cx6 + cx7 + cx8 + cx9 + cx10 + cx11)
summary(regLC)
anova(regLC)


center_data = cbind(cx1 , cx2 , cx3 , cx4 , cx5 , cx6 , cx7 , cx8 , cx9 , cx10 , cx11)

VIF=diag(solve(cor(center_data)))
VIF






############## Variable selection Forward ############

# Forward Selection
res0 = lm(y~ 1, data= wine)
res0
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data =wine, test = "F") 
# add x11 because lowest AIC and smallest p val and Highest F val

res1 = lm(y~ x11, data= wine)
add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x2 so we'll 
# add x2
summary(res1)

res2 = lm(y~ x11+x2, data= wine)
add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x10 so we'll 
# add x10
summary(res2)

res3 = lm(y~ x2+x10+x11, data= wine)
add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x4 so we'll 
# add x4
summary(res3)

res4 = lm(y~ x2+x4+x10+x11, data= wine)
add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x7 so we'll 
# add x7
summary(res4)

res5 = lm(y~ x2+x4+x7+x10+x11, data= wine)
add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x6 so we'll 
# add x6
summary(res5)

res6 = lm(y~x2+x4+x6+x7+x10+x11, data= wine)
add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x5 so we'll 
# add x5
summary(res6)

res7 = lm(y~ x2+x4+x5+x6+x7+x10+x11, data= wine)
add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x5 so we'll 
# add x5
summary(res7)

res8 = lm(y~ x2+x4+x5+x6+x7+x10+x11+x9, data= wine)
# now Smallest AIC, smallest p and highest F val  is for x9 so we'll 
# add x9
add1(res8, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
summary(res8)

##final model after forward selection: 
res8 = lm(y~ x2+x4+x5+x6+x7+x10+x11+x9, data= wine)



##############Back Propagation ############

# Backward selection
# Backward selection
res0 = lm(y~ x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x1 though AIC is small but p value is largest
# and F value is smallest

# Backward selection
res0 = lm(y~ x2+x3+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x3 though AIC is small but p value is largest
# and F value is smallest

# Backward selection
res0 = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x2+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x9 though AIC is small but p value is largest
# and F value is smallest

#final model after backward slection at alpha 0.1
res0 = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)



###################### Step-wise regression #######################
# Step-wise selection
res0 = lm(y~ 1, data=wine) 
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# x11 has smallest p largest F and smallest AIC add x11
res1 = lm(y~ x11, data= wine) 
# let's see do we need to drop x11 or not 
drop1(res1, y~x11, data = wine, test = "F")# no need to drop


add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res2 = lm(y~ x2+x11, data= wine) 
drop1(res2, y~x2+x11, data = wine, test = "F")# no need to drop

add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res3 = lm(y~ x2+x11+x10, data= wine) 
drop1(res3, y~x2+x11+x10, data = wine, test = "F")# no need to drop

add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res4 = lm(y~ x2+x11+x10+x4, data= wine) 
drop1(res4, y~x2+x11+x10+x4, data = wine, test = "F")# no need to drop

add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res5 = lm(y~ x2+x11+x10+x4+x7, data= wine) 
drop1(res5, y~x2+x11+x10+x4+x7, data = wine, test = "F")# no need to drop

add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res6 = lm(y~ x2+x11+x10+x4+x7+x6, data= wine) 
drop1(res6, y~x2+x11+x10+x4+x7+x6, data = wine, test = "F")# no need to drop

add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res7 = lm(y~ x2+x11+x10+x4+x7+x6+x5, data= wine) 
drop1(res7, y~x2+x11+x10+x4+x7+x6+x5, data = wine, test = "F")# no need to drop

add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res9 = lm(y~ x2+x11+x10+x4+x7+x6+x5+x9, data= wine) 
drop1(res9, y~x2+x11+x10+x4+x7+x6+x5+x9, data = wine, test = "F")# no need to drop

add1(res9, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
#we wont anything now

# the final model is  y~ x2+x11+x10+x4+x7+x6+x5+x9

## exhaustive search:
library(leaps)
all <- regsubsets(x=cbind(x2,x4,x5,x6,x7,x9,x10,x11), y=y,  method = "exhaustive", all.best = FALSE, nbest = 6)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which

p <- apply(Matrix,1, sum)
MSRes <- SSRes/(13-p)

# Make a nice table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output)[3:6] <- c("x2", "x4", "x5", "x6" , "x7" , "x9", "x10","x11") 
output
nrow(output)

label <- array(dim = c(43,8))

for (j in 1:43){
  if (output[j,3]=="1") label[j,1] = "x2" else label[j,1] = ""
  if (output[j,4]=="1") label[j,2] = paste(label[j,1], "x4", sep = "") else label[j,2] = label[j,1]	
  if (output[j,5]=="1") label[j,3] = paste(label[j,2], "x5", sep = "") else label[j,3] = label[j,2]	
  if (output[j,6]=="1") label[j,4] = paste(label[j,3], "x6", sep = "") else label[j,4] = label[j,3]
  if (output[j,7]=="1") label[j,5] = paste(label[j,4], "x7", sep = "") else label[j,5] = label[j,4]
  if (output[j,8]=="1") label[j,6] = paste(label[j,5], "x9", sep = "") else label[j,6] = label[j,5]
  if (output[j,9]=="1") label[j,7] = paste(label[j,6], "x10", sep = "") else label[j,7] = label[j,6]
  if (output[j,10]=="1") label[j,8] = paste(label[j,7], "x11", sep = "") else label[j,8] = label[j,7]
  
}
label[,8]

#uisng the msres r table 38
f1 = lm(y~ x2+x4+x5+x7+x9+x10+x11, data= wine)
Stud = rstudent(f1)
qqnorm(Stud)
qqline(Stud, col = "steelblue", lwd = 2)
plot(f1$fitted.values, Stud)
plot(f1) 
f2 = lm(y~ x2+x4+x5+x6+x7+x10+x11, data= wine)
Stud = rstudent(f2)
qqnorm(Stud)
qqline(Stud, col = "steelblue", lwd = 2)
plot(f2$fitted.values, Stud)
plot(f2)


library(calibrate)
plot(p, R2, pch = 16, main = "R2 plot")
textxy(p, R2, label[,8], cx = 0.8)

# Cp plot
plot(p, Cp, pch = 16, main = " Cp plot", ylim = c(0,6), xlim = c(0,6))
abline(0,1)
textxy(p, Cp, label[,8], cx = 0.8)

# MS_Res plot
plot(p, MSRes, pch = 16, main = " MS_Res plot")
textxy(p, MSRes, label[,4], cx = 0.8)

# Adjusted R2 plot
plot(p, AdjR2, pch = 16, main = "Cement - AdjR2 plot")
textxy(p, AdjR2, label[,4], cx = 0.8)


# Sanity check: fit the last model (with all 8 parameters) by hand and compare the output you get:
fit = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)
summary(fit)

# Obtain least squares parameter estimates for all models
coef(all, 1:43)









####################### Confidence Interval ####################
confint(fit)
#For all variables, the confidence interval does not include zero. 
# This implies that all variables are reliable.

