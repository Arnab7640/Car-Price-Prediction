rm(list=ls())

##Calling the Libraries

library(ppcor)
library(reshape2)
library(ggplot2)
library(moments)
library(MASS)
library(ggiraphExtra)
library(gridExtra)
library(snpar)
library(car)
library(writexl)
library(broom)
##Calling the dataset

setwd("C:/Users/USER/Desktop/DISSERTATION")
data=read.csv('Assignment.csv')
attach(data)

##Visualising the Response Variable

#i. Summary of the price dataset

d=summary(price)
d


#ii. Histogram of the Price of the Car

hist(price,freq = F,xlim=c(0,60000),xlab='price of cars(in dollar)',main='Histogram of Car price')
lines(density(price))

#ii. Boxplot of the Price of the Car

boxplot(price,ylab='price of cars(in dollar)',main='Boxplot of Car price')
legend('topright',legend =c('Min=5118','Q1=7788','Median=10295','Q3=16503','Max=45400'))

##Visualising the Categorical Data

##a> FUEL TYPE

theme_new=theme(plot.title=element_text(size=16,hjust = 0.5,face = 'bold.italic'),
                plot.subtitle=element_text(size=10,hjust = 0.5,face = 'bold.italic'),
                legend.title=element_text(hjust = 0.5,face = 'bold.italic',size=16),
                legend.text=element_text(size=14),axis.title=element_text(face='bold',size=16),
                axis.text=element_text(hjust=0.5,size=14))

 
p1=ggplot(NULL, aes(fueltype,fill=fueltype))+geom_bar(show.legend = F)+theme_new+labs(x='Fuel Type',y = 'Number of cars',title='Barplot of no. of cars w.r.t Fuel Type',fill='Index')

p2=ggplot(NULL,aes(fueltype,price,fill=fueltype))+geom_boxplot(show.legend = F)+theme_new+
  labs(title='Boxplot of  Car Price w.r.t. Fuel Type',x='Fuel Type',
       y=' price(in dollars)',col='Index')

grid.arrange(p1,p2,nrow=1)


##b> CAR COMPANY
par(mfrow=c(1,2))
y=table(Company)
y
barplot(y,cex.names =0.9,las=2,ylim=c(0,40),col=4:27,ylab='Number of cars',main='Barplot of no. of cars w.r.t. Car Company')
m2=aggregate(price,by=as.data.frame(Company),mean)
m2
barplot(m2[,2],names.arg=m2[,1],ylab='avg price(in dollars)',main='Barplot of avg car price w.r.t. Car Company',las=2,cex.names=0.9,cex.axis=0.8,ylim=c(0,50000),col=4:27)


###c> CAR TYPE

p1=ggplot(NULL,aes(carbody,fill=carbody))+geom_bar(show.legend = F)+
  labs( x='Car Type',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Car type')+theme_new
p2=ggplot(NULL, aes(carbody,price,fill=carbody))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Car Type',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Car Type')
grid.arrange(p1,p2,nrow=1)

###d> ENGINE TYPE

p1=ggplot(NULL,aes(enginetype,fill=enginetype))+geom_bar(show.legend = F)+
  labs( x='Engine Type',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Engine Type')+theme_new
p2=ggplot(NULL, aes(enginetype,price,fill=enginetype))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Engine Type',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Engine Type')
grid.arrange(p1,p2,nrow=1)


###e> DOOR NUMBER

p1=ggplot(NULL,aes(doornumber,fill=doornumber))+geom_bar(show.legend = F)+
  labs( x='Door Number',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Door Number')+theme_new
p2=ggplot(NULL, aes(doornumber,price,fill=doornumber))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Door Number',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Door Number')
grid.arrange(p1,p2,nrow=1)

###f> ASPIRATION


p1=ggplot(NULL,aes(aspiration,fill=aspiration))+geom_bar(show.legend = F)+
  labs( x='Aspiration',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Aspiration')+theme_new
p2=ggplot(NULL, aes(aspiration,price,fill=aspiration))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Aspiration',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Aspiration')
grid.arrange(p1,p2,nrow=1)

##g> ENGINE LOCATION

p1=ggplot(NULL,aes(enginelocation,fill=enginelocation))+geom_bar(show.legend = F)+
  labs( x='Engine Location',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Engine Location')+theme_new
p2=ggplot(NULL, aes(enginelocation,price,fill=enginelocation))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Engine Location',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Engine Location')
grid.arrange(p1,p2,nrow=1)


##h>CYLINDER NUMBER

p1=ggplot(NULL,aes(cylindernumber,fill=cylindernumber))+geom_bar(show.legend = F)+
  labs( x='Cylinder Number',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Cylinder Number')+theme_new
p2=ggplot(NULL, aes(cylindernumber,price,fill=cylindernumber))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Cylinder Number',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Cylinder Number')
grid.arrange(p1,p2,nrow=1)

##i>FUEL SYSTEM

p1=ggplot(NULL,aes(fuelsystem,fill=fuelsystem))+geom_bar(show.legend = F)+
  labs( x='Fuel System',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Fuel System')+theme_new
p2=ggplot(NULL, aes(fuelsystem,price,fill=fuelsystem))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Fuel System',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Fuel System')
grid.arrange(p1,p2,nrow=1)

##j>DRIVE WHEEL


p1=ggplot(NULL,aes(drivewheel,fill=drivewheel))+geom_bar(show.legend = F)+
  labs( x='Drive Wheel',y='Number of cars',
        title='Barplot of no. of cars w.r.t. Drive Wheel')+theme_new
p2=ggplot(NULL, aes(drivewheel,price,fill=drivewheel))+geom_boxplot(show.legend = F)+
  theme_new+labs(x='Drive Wheel',y='price(in dollars)',
                 title='Boxplot of  Car Price w.r.t. Drive Wheel')
grid.arrange(p1,p2,nrow=1)


###VISUALISING THE NUMERICAL DATA

theme_new1=theme(plot.title=element_text(size=16,hjust = 0.5,face = 'bold.italic'),
                 plot.subtitle=element_text(size=10,hjust = 0.5,face = 'bold.italic'),
                 legend.title=element_text(hjust = 0.5,face = 'bold.italic',size=16),
                 legend.text=element_text(size=14),axis.title=element_text(face='bold',size=16),
                 axis.text=element_text(hjust=0.5,size=12))
##a. Scatterplots
p1=ggplot(NULL,aes(wheelbase,price))+geom_point(col=2)+theme_new1
p2=ggplot(NULL,aes(carlength,price))+geom_point(col=3)+theme_new1
p3=ggplot(NULL,aes(carheight,price))+geom_point(col=4)+theme_new1
p4=ggplot(NULL,aes(curbweight,price))+geom_point(col=5)+theme_new1
p5=ggplot(NULL,aes(carwidth,price))+geom_point(col=6)+theme_new1
p6=ggplot(NULL,aes(enginesize,price))+geom_point(col=7)+theme_new1
p7=ggplot(NULL,aes(boreratio,price))+geom_point(col=10)+theme_new1
p8=ggplot(NULL,aes(stroke,price))+geom_point(col=11)+theme_new1
p9=ggplot(NULL,aes(compressionratio,price))+geom_point(col=12)+theme_new1
p10=ggplot(NULL,aes(peakrpm,price))+geom_point(col=13)+theme_new1
p11=ggplot(NULL,aes(horsepower,price))+geom_point(col=14)+theme_new1
p12=ggplot(NULL,aes(citympg,price))+geom_point(col=15)+theme_new1
p13=ggplot(NULL,aes(highwaympg,price))+geom_point(col=19)+theme_new1
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,ncol=4)

###b.CORRELATION HEATMAP
B1=data.frame(price,wheelbase,carlength,carheight,curbweight,carwidth,
              enginesize,boreratio,stroke,compressionratio,peakrpm,horsepower,citympg,highwaympg )

corr = data.matrix(cor(B1[sapply(B1,is.numeric)])) 
mel = melt(corr)
ggplot(mel, aes(Var1,Var2))+geom_tile(aes(fill=value)) +
  geom_text(aes(label = round(value,2)))+
  scale_fill_gradient2(low='yellow',mid='white' ,high='blue') + labs(title = 'Correlation Heatmap')+theme_new+
  theme(axis.text.x = element_text(angle=90))



B=data.frame(price,wheelbase,carlength,curbweight,carwidth,
             enginesize,boreratio,horsepower,citympg,highwaympg )
B2=B[,-1]
B2

##Partial Correlation Coefficients between 2 predictors ignoring the
##effect of other predictors
pcor(B2)
ggCor(B2,what=2,label=1,interactive = T,xangle=90)

##obtaining the skewness of response and explanatory variables

skewness(B[,-10])
#Reducing  the  skewness of highly skewed variables by taking log transformastion
A1=data.frame(log(price),log(wheelbase),(carlength),(curbweight),(carwidth),log(enginesize),(boreratio),log(horsepower),(citympg))
pcor(A1)
A1
A1


##Histograms showing the reduction of skewness
q1=ggplot(NULL,aes(price))+
  geom_histogram(fill=3,col=2,bins = 10,
                 aes(y=..density..))+theme_new
q2=ggplot(NULL,aes(log(price)))+
  geom_histogram(fill=3,col=2,bins = 10,
                 aes(y=..density..))+theme_new

q3=ggplot(NULL,aes(wheelbase))+
  geom_histogram(fill=4,col=2,bins = 10,
                 aes(y=..density..))+theme_new

q4=ggplot(NULL,aes(log(wheelbase)))+
  geom_histogram(fill=4,col=2,bins = 10,
                 aes(y=..density..))+theme_new

q5=ggplot(NULL,aes(enginesize))+
  geom_histogram(fill=5,col=2,bins = 10,
                 aes(y=..density..))+theme_new

q6=ggplot(NULL,aes(log(enginesize)))+
  geom_histogram(fill=5,col=2,bins = 10,
                 aes(y=..density..))+theme_new
q7=ggplot(NULL,aes(horsepower))+
  geom_histogram(fill=6,col=1,bins = 10,
                 aes(y=..density..))+theme_new
q8=ggplot(NULL,aes(log(horsepower)))+
  geom_histogram(fill=6,col=1,bins = 10,
                 aes(y=..density..))+theme_new

grid.arrange(q1,q2,q3,q4,q5,q6,q7,q8,ncol=2)

##Building the model
model1=lm(A1[,1]~drivewheel+cylindernumber+enginetype+carbody+aspiration+fueltype+A1[,2]+A1[,3]+A1[,4]+A1[,5]+A1[,6]+A1[,7]+A1[,8]+A1[,9])
summary(lm(A1[,1]~drivewheel+cylindernumber+enginetype+carbody+aspiration+fueltype+A1[,2]+A1[,3]+A1[,4]+A1[,5]+A1[,6]+A1[,7]+A1[,8]+A1[,9]))


s1=tidy(model1)


write_xlsx(s1,"C:/Users/USER/Desktop/DISSERTATION/s1.xlsx")
##Building a new model removing the non-significant explanatory variables
model=lm(A1[,1]~cylindernumber+enginetype+drivewheel+aspiration+carbody+fueltype+A1[,4]+A1[,5]+A1[,6]+A1[,8])

summary(model)
s2 =tidy(model)
write_xlsx(s2,"C:/Users/USER/Desktop/DISSERTATION/s2.xlsx")

##Obtaining the residuals
res=resid(model)
res

##Obtaining the predicted values of price
Y_hat=(fitted(model))
##Residual Plot
ggplot(NULL,aes(Y_hat,res))+geom_point(col=3,size=3)+labs(x='fitted values',y='residual',title = 'Residual Plot')+
  theme_new+geom_hline(yintercept = 0)
abline(h=0)



