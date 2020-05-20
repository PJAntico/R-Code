require(openxlsx)
require(xts)
require(Rblpapi)
require(dplyr)
require(faraway)
require(tibble)
require(reshape)
require(grid)
require(ggthemes)
require(MASS)
require(ggplot2)

options(scipen=999999)

############################################ Create the Data Set #######################################################
file = "C:/Users/pjant/Desktop/Machine-Learning-Python/Dataset.xlsx"

#Pull data in from excel files to create multiple data frames ordered by date
df_swapIV <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(2:3))
df_swapRates <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(5:6))
df_swapHV <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(5,7))
df_VIX <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(9:10))
df_curve <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(12:13))
df_SPX <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(15:16))
df_SPXHV <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(15,17))
df_Spreads <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(22:23))
df_HYG <- read.xlsx(file,sheet=1, detectDates=TRUE, startRow=2,cols=c(26:27))

#Combine these into one dataframe based on date
df<-Reduce(function(x,y) merge(x,y,all=TRUE),list(df_swapIV,df_swapRates,df_swapHV,df_VIX,df_curve,df_SPX,df_SPXHV,df_Spreads,df_HYG))

df$SWAP10.HV<-df$SWAP10.HV*100

#Filter for dates beginning after 2012-08-07 (this is when CDX was first quoted)
#df<-df%>%filter(Date>="2012-08-07")


############################################ Check for Missing Data ###################################################
colSums(is.na(df))
df[rowSums(is.na(df))>0,]
df[is.na(df$SWAP10)>0,] #all 4 dates are bond holidays
df[is.na(df$SWAP10.HV)>0,] #all 4 dates are the same bond holidays as above
df[is.na(df$SWAP10.IV)>0,]
df[is.na(df$VIX)>0,]
df[is.na(df$HY.Spds)>0,]

#Remove any rows with NA's
df<-na.omit(df)


############################################ Analyze the Data #########################################################
#Move the dates from being a column of data, to being the actual rownames
rownames(df)<-df[,1]
df<-df[-1]

#Rename the columns of the dataframe
colnames(df) <- c("IV", "SwapRate", "HV", "VIX", "Curve", "SPX", "SPXHV", "Spread", "HYG")
df = as.xts(df)

df_train = df["/20191108"]
df_test = df["20191111/"]

#analyze data
summary(df_train)
dim(df_train)
plot(as.data.frame(df_train))

############################################## Create the Model ######################################################
df2 <- as.data.frame(df_train)
df3 <- as.data.frame(df_test)

#check pairwise correlations of the predictors
#df2$IV=df2$IV/df2$SwapRate
lmodvol<-lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,data=df2)
summary(lmodvol)  #.8462


#Create the model without SPX and compare to the original model
#had to rename this or the boxcox function below will not run
lmodvol2<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+Spread+HYG,data=df2)
summary(lmodvol2) # .8225
summary(lmodvol)  # .7736

#SPXHV comes out as not a statistically significant variable - try building a model without this too and see how it looks
#lmodvol2a<-lm(IV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2)
#summary(lmodvol2a)
#summary(lmodvol2)

############################################## Transformations #######################################################

#boxcox transformation
par(mfrow=c(1,1))
boxcox(lmodvol,plotit=T)
bc=boxcox(lmodvol,plotit=T,lambda=seq(-1,1,by=.01))
bc$x[which.max(bc$y)] #Best transformation is the power of -.13
summary(lm(IV^-.13~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,data=df2))
#This model increases the adjusted r-squared from .8225 to .841, but SPXHV is not significant, so take it out
summary(lm(IV^-.13~SwapRate+HV+VIX+Curve+SPX+Spread+HYG,data=df2)) # .8409

#log transformations
logtrans(lmodvol,plotit=T)
lt=logtrans(lmodvol,plotit=T,alpha=seq(0,-48,by=-.1))
lt$x[which.max(lt$y)] #Alpha equal to 9 is optimal
summary(lm(log(IV+-13.1)~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,data=df2))
#This model increases the adjusted r-squared from .8225 to .8396, but SPXHV is not significant, so take it out
summary(lm(log(IV-13.1)~SwapRate+HV+VIX+Curve+SPX+Spread+HYG,data=df2)) # .8395


#New model
lmodvol3<-lm(IV^-.13~SwapRate+HV+VIX+Curve+SPX+Spread+HYG,data=df2)
summary(lmodvol3)
par(mfrow=c(1,1))

#############################################  Polynomials  ########################################################
#single polynomials
summary(lmodvol)  # .8462
summary(lm(IV~poly(SwapRate,degree=2)+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8531
summary(lm(IV~poly(SwapRate,degree=3)+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8595
summary(lm(IV~poly(SwapRate,degree=4)+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8613

summary(lm(IV~SwapRate+poly(HV,degree=2)+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8478
summary(lm(IV~SwapRate+poly(HV,degree=3)+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8478

summary(lm(IV~SwapRate+HV+poly(VIX,degree=2)+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8464
summary(lm(IV~SwapRate+HV+poly(VIX,degree=3)+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8466

summary(lm(IV~SwapRate+HV+VIX+poly(Curve,degree=2)+SPX+SPXHV+Spread+HYG,df2)) # .8482
summary(lm(IV~SwapRate+HV+VIX+poly(Curve,degree=3)+SPX+SPXHV+Spread+HYG,df2)) # .8495

summary(lm(IV~SwapRate+HV+VIX+Curve+poly(SPX,degree=2)+SPXHV+Spread+HYG,df2)) # .8511
summary(lm(IV~SwapRate+HV+VIX+Curve+poly(SPX,degree=3)+SPXHV+Spread+HYG,df2)) # .8555

summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+poly(SPXHV,degree=2)+Spread+HYG,df2)) # .8561
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+poly(SPXHV,degree=3)+Spread+HYG,df2)) # .8574

summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+poly(Spread,degree=2)+HYG,df2)) # .8514
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+poly(Spread,degree=3)+HYG,df2)) # .8622
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+poly(Spread,degree=4)+HYG,df2)) # .8649
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+poly(Spread,degree=5)+HYG,df2)) # .8717

summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+poly(HYG,degree=2),df2)) # .8474
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+poly(HYG,degree=3),df2)) # .8477
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+poly(HYG,degree=4),df2)) # .8485
summary(lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+Spread+poly(HYG,degree=5),df2)) # .8507

lmodvol4 <- lm(IV~poly(SwapRate,degree=4)+HV+VIX+Curve+SPX+SPXHV+Spread+HYG,df2) # .8613

#multiple polynomials
summary(lm(IV~polym(SwapRate,HV,degree=2)+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8564
summary(lm(IV~polym(SwapRate,HV,degree=3)+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8627
summary(lm(IV~polym(SwapRate,HV,degree=4)+VIX+Curve+SPX+SPXHV+Spread+HYG,df2)) # .8668

summary(lm(IV~polym(SwapRate,VIX,degree=2)+HV+Curve+SPX+Spread+HYG,df2)) # .8534
summary(lm(IV~polym(SwapRate,VIX,degree=3)+HV+Curve+SPX+Spread+HYG,df2)) # .87
summary(lm(IV~polym(SwapRate,VIX,degree=4)+HV+Curve+SPX+Spread+HYG,df2)) # .8781

summary(lm(IV~polym(SwapRate,Curve,degree=2)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .8858
summary(lm(IV~polym(SwapRate,Curve,degree=3)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .8943
summary(lm(IV~polym(SwapRate,Curve,degree=4)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .9013
summary(lm(IV~polym(SwapRate,Curve,degree=5)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .9133
summary(lm(IV~polym(SwapRate,Curve,degree=6)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .9246
summary(lm(IV~polym(SwapRate,Curve,degree=7)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .9347
summary(lm(IV~polym(SwapRate,Curve,degree=8)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)) # .9398****

summary(lm(IV~polym(SwapRate,SPXHV,degree=2)+HV+Curve+VIX+SPX+Spread+HYG,df2)) # .8697
summary(lm(IV~polym(SwapRate,SPXHV,degree=3)+HV+Curve+VIX+SPX+Spread+HYG,df2)) # .8725

summary(lm(IV~polym(SwapRate,HYG,degree=2)+HV+Curve+VIX+SPX+SPXHV+Spread,df2)) # .8557
summary(lm(IV~polym(SwapRate,HYG,degree=3)+HV+Curve+VIX+SPX+SPXHV+Spread,df2)) # .8655
summary(lm(IV~polym(SwapRate,HYG,degree=4)+HV+Curve+VIX+SPX+SPXHV+Spread,df2)) # .8842
summary(lm(IV~polym(SwapRate,HYG,degree=5)+HV+Curve+VIX+SPX+SPXHV+Spread,df2)) # .8917

summary(lm(IV~polym(SwapRate,Spread,degree=2)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .8541
summary(lm(IV~polym(SwapRate,Spread,degree=3)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .8789
summary(lm(IV~polym(SwapRate,Spread,degree=4)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .8867
summary(lm(IV~polym(SwapRate,Spread,degree=5)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .9047
summary(lm(IV~polym(SwapRate,Spread,degree=6)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .9134
summary(lm(IV~polym(SwapRate,Spread,degree=7)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .9188
summary(lm(IV~polym(SwapRate,Spread,degree=8)+HV+VIX+Curve+SPX+SPXHV+HYG,df2)) # .9219


lmodvol6 <- lm(IV~poly(SwapRate,Curve,degree=8)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)

###################################################  Prediction  ######################################################
#pred=t(c(1,1.8315,42.018,12.7,.13,329.6))%*%coef(lmodvol)
#pred=pred
#pred


#Pull data in from excel files to create multiple data frames ordered by date
#df3_swapIV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(2:3))
#df3_swapRates <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(5:6))
#df3_swapHV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(5,7))
#df3_VIX <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(9:10))
#df3_curve <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(12:13))
#df3_SPX <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(15:16))
#df3_SPXHV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(15,17))
#df3_CDXHY <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(19:20))
#df3_Spreads <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(22:23))

#Combine these into one dataframe based on date
#df3<-Reduce(function(x,y) merge(x,y,all=TRUE),list(df3_swapIV,df3_swapRates,df3_swapHV,df3_VIX,df3_curve,df3_SPX,
#                                                   df3_SPXHV,df3_CDXHY,df3_Spreads))

#df3$SWAP10.HV<-df3$SWAP10.HV*100

#df3<-na.omit(df3)

#Move the dates from being a column of data, to being the actual rownames
#rownames(df3)<-df3[,1]
#df3<-df3[-1]

#Rename the columns of the dataframe
#colnames(df3) <- c("IV", "SwapRate", "HV", "VIX", "Curve", "SPX", "SPXHV", "CDXHY", "Spread")
#df3$IV=df3$IV/df3$SwapRate

pred=as.data.frame(cbind(predict(lmodvol,df3, interval="prediction"),df3$IV))
colnames(pred)[4]="actual"

pred3=as.data.frame(cbind(predict(lmodvol3,subset(df3,select=-c(SPXHV)),interval="prediction"),df3$IV))
colnames(pred3)[4]="actual"

pred4=as.data.frame(cbind(predict(lmodvol4,df3, interval="prediction"),df3$IV))
colnames(pred4)[4]="actual"

#lmodvol6 = lm(IV~poly(SwapRate,Curve,degree=8)+HV+VIX+SPX+SPXHV+Spread+HYG,df2)
#head(predict(lmodvol6,df3,interval="prediction"))

pred6=as.data.frame(cbind(predict(lmodvol6,df3, interval="prediction"),df3$IV))
colnames(pred6)[4]="actual"


predictions=as.data.frame(cbind(as.Date(rownames(pred),format="%Y-%m-%d"),round(pred$fit,3),round(pred3$fit,3),round(pred4$fit,3),
                                round(pred6$fit,3),round(df3$IV,3)))


#rownames(predictions)=rownames(pred2)
colnames(predictions)=c("Dates","SimpleModel","TransformedModel", "SinglePolyModel","Multiple Polynomial","Actual")
predictions[,1]=as.Date(predictions[,1],origin="1970-01-01")
predictions[,3]=(predictions$TransformedModel^(1/-.13))
head(predictions)


predictions_long=melt(predictions,id="Dates")
colnames(predictions_long)[2]="Model"

ggplot(data=predictions_long,aes(x=Dates,y=value,color=Model))+
  geom_line(aes(linetype=Model,color=Model,size=Model))+
  ylim(0,150)+
  scale_x_date(date_breaks="1 week")+
  #  theme_bw()+
  #  theme_stata()+
  #  scale_color_stata("s1color")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(x="Dates\n",y="IV Values\n")+
  scale_color_manual(values=c("red","blue","dark green","orange","black"))+
  scale_linetype_manual(values=c("dashed","dashed","dashed","dashed","solid"))+
  scale_size_manual(values=c(1,1,1,1,1))+
  ggtitle("Actual IV Values v Predicted IV Values\n")+
  theme(plot.title=element_text(size=24,hjust=0.5),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        legend.title=element_text(size=16),
        legend.text=element_text(size=10))
#  geom_point(aes(x=as.Date("2019-11-20"),y=40),data=predictions_long,size=25,shape=1,color="grey24",stroke=1)+
#  geom_point(aes(x=as.Date("2020-04-03"),y=42.5),data=predictions_long,size=25,shape=1,color="grey24",stroke=1)
