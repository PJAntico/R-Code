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


options(scipen=999999)

############################################ Create the Data Set #######################################################
file = "C:/Users/pjant/Drew/DATA 601/datasets/final project/Dataset for Final Project.xlsx"

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
colnames(df) <- c("IV", "SwapRate", "HV", "VIX", "Curve", "SPX", "SPXHV", "CDXHY", "Spread")
df = as.xts(df)

df_train = df["/20191108"]
df_test = df["20191111/"]

#analyze data
summary(df_train)
dim(df_train)
plot(as.data.frame(df_train))

############################################ Check for Collinearity ###################################################
df2 <- as.data.frame(df_train)

#check pairwise correlations of the predictors
df2$IV=df2$IV/df2$SwapRate
lmodvol<-lm(IV~SwapRate+HV+VIX+Curve+SPX+SPXHV+CDXHY+Spread,data=df2)

dfm<-model.matrix(lmodvol)[,-1]
head(dfm)
cor(dfm)

#check for collinearity via eigendecomposition and VIF's
ev<-eigen(crossprod(dfm))
ev$val
(K=sqrt(ev$val[1]/ev$val)) #6 values are too large - need to normalize all values - move on to VIF's


#VIFs
vif(dfm)
vif(dfm[,-5]) #remove SPX

############################################## Create the Model ######################################################

#Create the model without SPX and compare to the original model
#had to rename this or the boxcox function below will not run
lmodvol2<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)
summary(lmodvol2)
summary(lmodvol)

#SPXHV comes out as not a statistically significant variable - try building a model without this too and see how it looks
#lmodvol2a<-lm(IV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2)
#summary(lmodvol2a)
#summary(lmodvol2)


###################################### Residual v. Predictor Plots ######################################################
#Residuals v. Predictors
model = lmodvol
par(mfrow=c(2,5))
plot(df2$IV,residuals(model), xlab="Implied Vol", ylab="Residuals")
abline(h=0)
plot(df2$SwapRate,residuals(model), xlab="10Y Swap Rate", ylab="Residuals")
abline(h=0)
plot(df2$HV,residuals(model), xlab="10Y Swap Rate HV", ylab="Residuals")
abline(h=0)
plot(df2$VIX,residuals(model), xlab="VIX", ylab="Residuals")
abline(h=0)
plot(df2$Curve,residuals(model), xlab="Curve", ylab="Residuals")
abline(h=0)
plot(df2$SPX,residuals(model), xlab="SPX", ylab="Residuals")
abline(h=0)
plot(df2$SPXHV,residuals(model), xlab="SPX Historical Vol", ylab="Residuals")
abline(h=0)
plot(df2$CDXHY,residuals(model), xlab="HY Spreads", ylab="Residuals")
abline(h=0)
plot(df2$Spread,residuals(model), xlab="Swap Spreads", ylab="Residuals")
abline(h=0)
par(mfrow=c(1,1))


############################################## Transformations #######################################################

#boxcox transformation
par(mfrow=c(1,1))
boxcox(lmodvol,plotit=T)
bc=boxcox(lmodvol,plotit=T,lambda=seq(0,-.5,by=-.01))
bc$x[which.max(bc$y)] #Best transformation is the power of .3838384
summary(lm(IV^-.3838384~SwapRate+HV+VIX+Curve+SPX+SPXHV+CDXHY+Spread,data=df2))
#This model increases the adjusted r-squared from .7903 to .9112

#log transformations
logtrans(lmodvol,plotit=T)
lt=logtrans(lmodvol,plotit=T,alpha=seq(-2.5,0,by=.1))
lt$x[which.max(lt$y)] #Alpha equal to -2.070707 is optimal
summary(lm(log(IV-2.070707)~SwapRate+HV+VIX+Curve+SPX+SPXHV+CDXHY+Spread,data=df2))
#This model increases the adjusted r-squared from .7903 to .9057, BUT CDXHY is not relevant

#New model
lmodvol3<-lm(IV^-.3838384~SwapRate+HV+VIX+Curve+SPX+SPXHV+CDXHY+Spread,data=df2)
lmodvol4<-lm(log(IV-2.070707)~SwapRate+HV+VIX+Curve+SPX+SPXHV+CDXHY+Spread,data=df2)
summary(lmodvol3)
summary(lmodvol4)
par(mfrow=c(1,1))

#lmodvol3 has the best combination of high r^2 and relevance of variables, and as such, is the "best" model so far

#############################################  Polynomials  ########################################################
#multiple polynomials
summary(lm(IV~polym(SwapRate,HV,degree=2)+VIX+Curve+SPXHV+CDXHY+Spread,df2)) # .882

summary(lm(IV~polym(SwapRate,VIX,degree=2)+HV+Curve+SPXHV+CDXHY+Spread,df2)) # .8836

summary(lm(IV~polym(SwapRate,Curve,degree=2)+HV+VIX+SPXHV+CDXHY+Spread,df2)) # .9093
summary(lm(IV~polym(SwapRate,Curve,degree=3)+HV+VIX+SPXHV+CDXHY+Spread,df2)) # .9152
summary(lm(IV~polym(SwapRate,Curve,degree=4)+HV+VIX+SPXHV+CDXHY+Spread,df2)) # .9362
summary(lm(IV~polym(SwapRate,Curve,degree=5)+HV+VIX+SPXHV+CDXHY+Spread,df2)) # .9444
summary(lm(IV~polym(SwapRate,Curve,degree=6)+HV+VIX+SPXHV+CDXHY+Spread,df2)) # .9482***

summary(lm(IV~polym(SwapRate,SPXHV,degree=2)+HV+VIX+Curve+CDXHY+Spread,df2)) # .8823
summary(lm(IV~polym(SwapRate,SPXHV,degree=3)+HV+VIX+Curve+CDXHY+Spread,df2)) # .883

summary(lm(IV~polym(SwapRate,CDXHY,degree=2)+HV+VIX+Curve+SPXHV+Spread,df2)) # .8908
summary(lm(IV~polym(SwapRate,CDXHY,degree=3)+HV+VIX+Curve+SPXHV+Spread,df2)) # .899
summary(lm(IV~polym(SwapRate,CDXHY,degree=4)+HV+VIX+Curve+SPXHV+Spread,df2)) # .9026

summary(lm(IV~polym(SwapRate,Spread,degree=2)+HV+VIX+Curve+SPXHV+CDXHY,df2)) # .8953
summary(lm(IV~polym(SwapRate,Spread,degree=3)+HV+VIX+Curve+SPXHV+CDXHY,df2)) # .9152
summary(lm(IV~polym(SwapRate,Spread,degree=4)+HV+VIX+Curve+SPXHV+CDXHY,df2)) # .9221
summary(lm(IV~polym(SwapRate,Spread,degree=5)+HV+VIX+Curve+SPXHV+CDXHY,df2)) # .9285
summary(lm(IV~polym(SwapRate,Spread,degree=6)+HV+VIX+Curve+SPXHV+CDXHY,df2)) # .9374

lmodvol6 <- lm(IV~polym(SwapRate,Curve,degree=6)+HV+VIX+SPXHV+CDXHY+Spread,df2)
plot(lmodvol6)

#Plot the 1) original model, 2) the best transformed response model, 3) the best single polynomial model, and 4) the best multiple polynomial model
par(mfrow=c(2,2))
plot(lmodvol2)
plot(lmodvol3)  # .8719 r-squared, negatively skewed Q-Q plot, not "complex"
plot(lmodvol5)  # .8819 r-squared, slightly positively skewed Q-Q plot, not "complex"
plot(lmodvol6)  # .9374 r-squared, LONG-TAILED distribution, worst "residuals v. leverage" plot of the 3, "complex" 
par(mfrow=c(2,2))

#Check that the curvature in the Residuals v. SwapRate predictor is no longer there in the newer models
par(mfrow=c(2,2))
plot(df2$SwapRate,residuals(lmodvol2), xlab="10Y Swap Rate", ylab="Residuals", main="Simple Model")
abline(h=0)
plot(df2$SwapRate,residuals(lmodvol3), xlab="10Y Swap Rate", ylab="Residuals", main="Transformed Model")
abline(h=0)
plot(df2$SwapRate,residuals(lmodvol5), xlab="10Y Swap Rate", ylab="Residuals", main="Single Polynomial Model")
abline(h=0)
plot(df2$SwapRate,residuals(lmodvol6), xlab="10Y Swap Rate", ylab="Residuals", main="Multiple Polynomial Model")
abline(h=0)
par(mfrow=c(1,1))


######################################### Constant Variance #########################################################

#Plot the Residuals of all of the models and compare (have to work with the residuals because we have no access to the errors)
#looking for constant symetrical variation (homoscedasticity) in the vertical direction, and nonlinearity
par(mfrow=c(2,2))
plot(fitted(lmodvol2),residuals(lmodvol),xlab="Fitted",ylab="Residuals",main="Simple model")
abline(h=0)
plot(fitted(lmodvol3),residuals(lmodvol3),xlab="Fitted",ylab="Residuals",main="Transformed Model")
abline(h=0)
plot(fitted(lmodvol5),residuals(lmodvol5),xlab="Fitted",ylab="Residuals",main="Single Polynomial Model")
abline(h=0)
plot(fitted(lmodvol6),residuals(lmodvol6),xlab="Fitted",ylab="Residuals",main="Multiple Polynomial Model")
abline(h=0)
par(mfrow=c(1,1))


#effectively double the resolution by connsidering the absolute value of the residuals
#and then do a quick numerical tst to check nonconstant variance
par(mfrow=c(2,2))
plot(fitted(lmodvol2),sqrt(abs(residuals(lmodvol))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))),main="Simple Model")
summary(lm(sqrt(abs(residuals(lmodvol)))~fitted(lmodvol))) #predictor is the fitted values
plot(fitted(lmodvol3),sqrt(abs(residuals(lmodvol3))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))),main="Transformed Model")
summary(lm(sqrt(abs(residuals(lmodvol3)))~fitted(lmodvol3))) #predictor is the fitted values
plot(fitted(lmodvol5),sqrt(abs(residuals(lmodvol5))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))), main="Single Polynomial Model")
summary(lm(sqrt(abs(residuals(lmodvol5)))~fitted(lmodvol5))) #predictor is the fitted values
plot(fitted(lmodvol6),sqrt(abs(residuals(lmodvol6))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))), main="Multiple Polynomial Model")
summary(lm(sqrt(abs(residuals(lmodvol6)))~fitted(lmodvol6))) #predictor is the fitted values
par(mfrow=c(1,1))


########################################## Normality #################################################################
#Plot the standardized residuals for each of the models to compare
par(mfrow=c(2,2))
qqnorm(residuals(lmodvol2),ylab="Residuals", main="Simple Model")
qqline(residuals(lmodvol2))
qqnorm(residuals(lmodvol3),ylab="Residuals", main="Transformed Model")
qqline(residuals(lmodvol3))
qqnorm(residuals(lmodvol5),ylab="Residuals", main="Single Polynomial Model")
qqline(residuals(lmodvol5))
qqnorm(residuals(lmodvol6),ylab="Residuals", main="Multiple Polynomial Model")
qqline(residuals(lmodvol6))
par(mfrow=c(1,1))

#Run a histogram for each of the models
par(mfrow=c(2,2))
hist(residuals(lmodvol2),xlab="Residuals",main="Simple Model")
hist(residuals(lmodvol3),xlab="Residuals",main="Transformed Model")
hist(residuals(lmodvol5),xlab="Residuals",main="Single Polynomial Model")
hist(residuals(lmodvol6),xlab="Residuals",main="Multiple Polynomial Model")
par(mfrow=c(1,1))

#Run a shapiro test for each of the models
shapiro.test(residuals(lmodvol2))
shapiro.test(residuals(lmodvol3))
shapiro.test(residuals(lmodvol5))
shapiro.test(residuals(lmodvol6))
#None are normal

########################################## Leverages #################################################################
n=nrow(df2)
p=length(coef(lmodvol2))

#lmodvol
hatv2<-hatvalues(lmodvol2)
head(hatv2)
sum(hatv2)
2*p/n
large_hatv2<-data.frame(hatv2[hatv2>2*p/n])
colnames(large_hatv2)<-c("hatv2")
large_hatv2$date=as.character(rownames(large_hatv2))
large_hatv2<-large_hatv2[,c(2,1)]
# largest_hatv<-large_hatv[order(-large_hatv$hatv),,drop=FALSE]
dim(large_hatv2)

#lmodvol3
hatv3<-hatvalues(lmodvol3)
head(hatv3)
sum(hatv3)
2*p/n
large_hatv3<-data.frame(hatv3[hatv3>2*p/n])
colnames(large_hatv3)<-c("hatv3")
large_hatv3$date=as.character(rownames(large_hatv3))
large_hatv3<-large_hatv3[,c(2,1)]
# largest_hatv3<-large_hatv3[order(-large_hatv3$hatv),,drop=FALSE]
dim(large_hatv3)

#lmodvol5
hatv5<-hatvalues(lmodvol5)
head(hatv5)
sum(hatv5)
2*p/n
large_hatv5<-data.frame(hatv5[hatv5>2*p/n])
colnames(large_hatv5)<-c("hatv5")
large_hatv5$date=as.character(rownames(large_hatv5))
large_hatv5<-large_hatv5[,c(2,1)]
# large_hatv5<-large_hatv5[order(-large_hatv5$hatv),,drop=FALSE]
dim(large_hatv5)

#lmodvol6
hatv6<-hatvalues(lmodvol6)
head(hatv6)
sum(hatv6)
2*p/n
large_hatv6<-data.frame(hatv6[hatv6>2*p/n])
colnames(large_hatv6)<-c("hatv6")
large_hatv6$date=as.character(rownames(large_hatv6))
large_hatv6<-large_hatv6[,c(2,1)]
# largest_hatv6<-large_hatv6[order(-large_hatv6$hatv),,drop=FALSE]
dim(large_hatv6)

#compare summaries of all the largest hatvalues for each model
large_hatv_table<-full_join(large_hatv2, large_hatv3, by="date")
large_hatv_table<-full_join(large_hatv_table, large_hatv5, by="date")
large_hatv_table<-large_hatv_table%>%arrange(desc(hatv2))
large_hatv_table<-na.omit(large_hatv_table[large_hatv_table$hatv2>4*p/n | large_hatv_table$hatv3>4*p/n | large_hatv_table$hatv5>4*p/n,])
large_hatv_table<-large_hatv_table%>%arrange(desc(date))

#for export to .csv files
large_hatv_table[,2:4]<-round(large_hatv_table[,2:4],4)
dim(large_hatv_table)
write.csv(large_hatv_table, "C:/Users/pjant/Drew/DATA 503/large_hatv_table.csv")

#look into each of the 5 periods of high hatvalues
#period 1
df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2013-06-25" & date<="2013-07-19")%>%
  column_to_rownames(var="date")

df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2015-08-20" & date<="2015-08-30")%>%
  column_to_rownames(var="date")

df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2016-06-22" & date<="2016-07-06")%>%
  column_to_rownames(var="date")

df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2018-02-01" & date<="2018-02-10")%>%
  column_to_rownames(var="date")

df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2018-12-15" & date<="2018-12-30")%>%
  column_to_rownames(var="date")



########################################## Outliers ##################################################################
#looking for points that do not fit the model well (have a large residual)

#lmodvol outlier analysis
#Studentized residuals for lmodvol
stud2=rstudent(lmodvol2)
(tt=stud2[which.max(abs(stud2))])
stud2<-data.frame(stud2)
colnames(stud2)<-c("stud2")
stud2$date=as.character(rownames(stud2))
stud2<-stud2[,c(2,1)]
#Bonferroni critical value
bv<-qt(1-(.05/(1807*2)),1807-9-1)
# Bonferroni critical value is 4.202695; highest studentized residual is 9/4/2013, at 3.58657 - NOT an outlier
#new alpha
alpha2=.05/n
#Calculate the p-value: 2*P(t>tt)
2*(1-pt(abs(tt),n-p-1)); alpha2  #p-value is greater than alpha2, so not an outlier

#lmodvol3 outlier analysis
#Studentized residuals for lmodvol3
stud3=rstudent(lmodvol3)
(tt3=stud3[which.max(abs(stud3))])
stud3<-data.frame(stud3)
colnames(stud3)<-c("stud3")
stud3$date=as.character(rownames(stud3))
stud3<-stud3[,c(2,1)]
#Bonferroni critical value
bv3<-qt(1-(.05/(1807*2)),1807-9-1)
# Bonferroni critical value is 4.202695; highest studentized residual is 9/4/2013, at -4.02483 - NOT an outlier, but close
#Calculate the p-value: 2*P(t>tt)
2*(1-pt(abs(tt3),n-p-1)); alpha2  #p-value is greater than alpha2, so not an outlier

#lmodvol5 outlier analysis
#Studentized residuals for lmodvol5
stud5=rstudent(lmodvol5)
(tt5=stud5[which.max(abs(stud5))])
stud5<-data.frame(stud5)
colnames(stud5)<-c("stud5")
stud5$date=as.character(rownames(stud5))
stud5<-stud5[,c(2,1)]
#Bonferroni critical value
bv5<-qt(1-(.05/(1807*2)),1807-9-1)
# Bonferroni critical value is 4.202695; highest studentized residual is 7/3/2013, at 4.314365 - IS an outlier
#Calculate the p-value: 2*P(t>tt)
2*(1-pt(abs(tt5),n-p-1)); alpha2 #p-value is less than alpha2, so is an outlier

#Just as in the review for Leverage points in the section above, lets put together a table of the highest
#Bonferroni corrections in each model and analyze them
#compare summaries of all the largest hatvalues for each model
stud_table<-full_join(stud2, stud3, by="date")
stud_table<-full_join(stud_table, stud5, by="date")
stud_table<-stud_table%>%arrange(desc(abs(stud2)))
stud_table<-na.omit(stud_table[abs(stud_table$stud2)>(bv*.85) | abs(stud_table$stud3)>(bv3*.85) | abs(stud_table$stud5)>(bv5*.85),])
stud_table<-stud_table%>%arrange(desc(date))

#for export to .csv file
stud_table[,2:4]<-round(stud_table[,2:4],3)
dim(stud_table)
write.csv(stud_table, "C:/Users/pjant/Drew/DATA 503/stud_table.csv")

#look into each of the 5 large studentized residuals
df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2013-08-25" & date<="2013-09-10")%>%
  column_to_rownames(var="date")

df2 %>%
  rownames_to_column(var="date") %>%
  filter(date>="2013-06-15" & date<="2013-07-19")%>%
  column_to_rownames(var="date")



########################################## Influential Observations ###################################################
#Run Cook's Distances for all 3 models

cook2=data.frame(cooks.distance(lmodvol2))
cook3=data.frame(cooks.distance(lmodvol3))
cook5=data.frame(cooks.distance(lmodvol5))

colnames(cook2)="cook_dist2"
colnames(cook3)="cook_dist3"
colnames(cook5)="cook_dist5"

par(mfrow=c(2,2))
plot(cook2$cook_dist,xlab="Observation Number", ylab="Cook's Distance", main="Simple Model")
#text(which(cook2$cook_dist2>=.02),labels=rownames(cook2[which(cook2$cook_dist2>.02),,drop=FALSE]), pos=4)
plot(cook3$cook_dist,xlab="Observation Number", ylab="Cook's Distance", main="Transformed Model")
plot(cook5$cook_dist,xlab="Observation Number", ylab="Cook's Distance", main="Single Polynomial Model")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
halfnorm(cook2$cook_dist,3,labs=rownames(cook2),ylab="Cook's distances",main="Simple Model")
halfnorm(cook3$cook_dist,3,labs=rownames(cook3),ylab="Cook's distances",main="Transformed Model")
halfnorm(cook5$cook_dist,3,labs=rownames(cook5),ylab="Cook's distances",main="Single Polynomial Model")
par(mfrow=c(1,1))

tail(sort(cook2$cook_dist))
tail(sort(cook3$cook_dist))
tail(sort(cook5$cook_dist))

#identify the dates and value of the largest Cook's Distances for each model (for use in the Final Project Report)
head(cook2 %>% 
  mutate(Date=rownames(cook2))%>%
  arrange(desc(cook_dist2)))

head(cook3 %>% 
       mutate(Date=rownames(cook3))%>%
       arrange(desc(cook_dist3)))

head(cook5 %>% 
       mutate(Date=rownames(cook5))%>%
       arrange(desc(cook_dist5)))



cases2=which(cook2$cook_dist>.02)
cases3=which(cook3$cook_dist>.02)
cases5=which(cook5$cook_dist>.02)

cook2$cook_dist[cases2]
cook3$cook_dist[cases3]
cook5$cook_dist[cases5]


#What happens if we leave the three most influential points out, or just the most influential one
#Simple model
lmodvol2_a<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2[-cases2,])
lmodvol2_b<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2,subset=cook2$cook_dist2<max(cook2$cook_dist2))
summary(lmodvol2)
summary(lmodvol2_a)
summary(lmodvol2_b)

lmodvol3_a<-lm(IV^-.25~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2[-cases3,])
lmodvol3_b<-lm(IV^-.25~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2,subset=cook3$cook_dist3<max(cook3$cook_dist3))
summary(lmodvol3)
summary(lmodvol3_a)
summary(lmodvol3_b)

lmodvol5_a<-lm(IV~poly(SwapRate,2)+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2[-cases5,])
lmodvol5_b<-lm(IV~poly(SwapRate,2)+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2,subset=cook5$cook_dist5<max(cook5$cook_dist5))
summary(lmodvol5)
summary(lmodvol5_a)
summary(lmodvol5_b)


#Show all the plots provided by R
par(mfrow=c(2,2))
plot(lmodvol2)
plot(lmodvol3)
plot(lmodvol5)
par(mfrow=c(1,1))

#################################### Check the Structure of the Model #################################################
#Run Partial Regression Plots for all 3 of the models, for every variable

#Simple Model
#Obtain a version of Y with the effect of all predictors but one
d1=residuals(lm(IV~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2))       #exclude SwapRate
d2=residuals(lm(IV~SwapRate+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) #exclude HV
d3=residuals(lm(IV~SwapRate+HV+Curve+SPXHV+CDXHY+Spread,data=df2))  #exclude VIX
d4=residuals(lm(IV~SwapRate+HV+VIX+SPXHV+CDXHY+Spread,data=df2))    #exclude Curve
d5=residuals(lm(IV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2))    #exclude SPXHV
d6=residuals(lm(IV~SwapRate+HV+VIX+Curve+SPXHV+Spread,data=df2))    #exclude CDXHY
d7=residuals(lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2))     #exclude Spread


#Obtain a version of each predictor with the effect of all of the other predictors
m1=residuals(lm(SwapRate~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m2=residuals(lm(HV~SwapRate+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m3=residuals(lm(VIX~SwapRate+HV+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m4=residuals(lm(Curve~SwapRate+HV+VIX+SPXHV+CDXHY+Spread,data=df2))
m5=residuals(lm(SPXHV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2))
m6=residuals(lm(CDXHY~SwapRate+HV+VIX+Curve+SPXHV+Spread,data=df2))
m7=residuals(lm(Spread~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2))

#Partial regression plot:
par(mfrow=c(2,4))

plot(m1,d1,xlab="10y Swap residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Swap Rate
abline(0,coef(lmodvol2)['SwapRate'])
abline(lm(d1~m1), col=2)

plot(m2,d2,xlab="HV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for HV
abline(0,coef(lmodvol2)['HV'])
abline(lm(d2~m2), col=2)

plot(m3,d3,xlab="VIX residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for VIX
abline(0,coef(lmodvol2)['VIX'])
abline(lm(d3~m3), col=2)

plot(m4,d4,xlab="Curve residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Curve
abline(0,coef(lmodvol2)['Curve'])
abline(lm(d4~m4), col=2)

plot(m5,d5,xlab="SPXHV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for SPXHV
abline(0,coef(lmodvol2)['SPXHV'])
abline(lm(d5~m5), col=2)

plot(m6,d6,xlab="CDXHY residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for CDXHY
abline(0,coef(lmodvol2)['CDXHY'])
abline(lm(d6~m6), col=2)

plot(m7,d7,xlab="Spread residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Spread
abline(0,coef(lmodvol2)['Spread'])
abline(lm(d7~m7), col=2)


#Same coefficients
coef(lm(d1~m1))
coef(lm(d2~m2))
coef(lm(d3~m3))
coef(lm(d4~m4))
coef(lm(d5~m5))
coef(lm(d6~m6))
coef(lm(d7~m7))
coef(lmodvol2)


#Transformed Model
#Obtain a version of Y with the effect of all predictors but one
d8=residuals(lm(IV^-.25~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2))       #exclude SwapRate
d9=residuals(lm(IV^-.25~SwapRate+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) #exclude HV
d10=residuals(lm(IV^-.25~SwapRate+HV+Curve+SPXHV+CDXHY+Spread,data=df2))  #exclude VIX
d11=residuals(lm(IV^-.25~SwapRate+HV+VIX+SPXHV+CDXHY+Spread,data=df2))    #exclude Curve
d12=residuals(lm(IV^-.25~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2))    #exclude SPXHV
d13=residuals(lm(IV^-.25~SwapRate+HV+VIX+Curve+SPXHV+Spread,data=df2))    #exclude CDXHY
d14=residuals(lm(IV^-.25~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2))     #exclude Spread


#Obtain a version of each predictor with the effect of all of the other predictors
m8=residuals(lm(SwapRate~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m9=residuals(lm(HV~SwapRate+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m10=residuals(lm(VIX~SwapRate+HV+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m11=residuals(lm(Curve~SwapRate+HV+VIX+SPXHV+CDXHY+Spread,data=df2))
m12=residuals(lm(SPXHV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2))
m13=residuals(lm(CDXHY~SwapRate+HV+VIX+Curve+SPXHV+Spread,data=df2))
m14=residuals(lm(Spread~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2))

#Partial regression plot:
par(mfrow=c(2,4))

plot(m8,d8,xlab="10y Swap residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Swap Rate
abline(0,coef(lmodvol3)['SwapRate'])
abline(lm(d8~m8), col=2)

plot(m9,d9,xlab="HV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for HV
abline(0,coef(lmodvol3)['HV'])
abline(lm(d9~m9), col=2)

plot(m10,d10,xlab="VIX residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for VIX
abline(0,coef(lmodvol3)['VIX'])
abline(lm(d10~m10), col=2)

plot(m11,d11,xlab="Curve residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Curve
abline(0,coef(lmodvol3)['Curve'])
abline(lm(d11~m11), col=2)

plot(m12,d12,xlab="SPXHV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for SPXHV
abline(0,coef(lmodvol3)['SPXHV'])
abline(lm(d12~m12), col=2)

plot(m13,d13,xlab="CDXHY residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for CDXHY
abline(0,coef(lmodvol3)['CDXHY'])
abline(lm(d13~m13), col=2)

plot(m14,d14,xlab="Spread residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Spread
abline(0,coef(lmodvol3)['Spread'])
abline(lm(d14~m14), col=2)


#Same coefficients
coef(lm(d8~m8))
coef(lm(d9~m9))
coef(lm(d10~m10))
coef(lm(d11~m11))
coef(lm(d12~m12))
coef(lm(d13~m13))
coef(lm(d14~m14))
coef(lmodvol3)



#Single Polynomial Model
#Obtain a version of Y with the effect of all predictors but one
d15=residuals(lm(IV~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2))               #exclude SwapRate
d16=residuals(lm(IV~poly(SwapRate,2)+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) #exclude HV
d17=residuals(lm(IV~poly(SwapRate,2)+HV+Curve+SPXHV+CDXHY+Spread,data=df2)) #exclude VIX
d18=residuals(lm(IV~poly(SwapRate,2)+HV+VIX+SPXHV+CDXHY+Spread,data=df2))   #exclude Curve
d19=residuals(lm(IV~poly(SwapRate,2)+HV+VIX+Curve+CDXHY+Spread,data=df2))   #exclude SPXHV
d20=residuals(lm(IV~poly(SwapRate,2)+HV+VIX+Curve+SPXHV+Spread,data=df2))   #exclude CDXHY
d21=residuals(lm(IV~poly(SwapRate,2)+HV+VIX+Curve+SPXHV+CDXHY,data=df2))    #exclude Spread


#Obtain a version of each predictor with the effect of all of the other predictors
m15=residuals(lm(SwapRate~HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m16=residuals(lm(HV~SwapRate+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m17=residuals(lm(VIX~SwapRate+HV+Curve+SPXHV+CDXHY+Spread,data=df2)) 
m18=residuals(lm(Curve~SwapRate+HV+VIX+SPXHV+CDXHY+Spread,data=df2))
m19=residuals(lm(SPXHV~SwapRate+HV+VIX+Curve+CDXHY+Spread,data=df2))
m20=residuals(lm(CDXHY~SwapRate+HV+VIX+Curve+SPXHV+Spread,data=df2))
m21=residuals(lm(Spread~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2))

#Partial regression plot:
par(mfrow=c(2,4))

plot(m15,d15,xlab="10y Swap residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Swap Rate
abline(0,coef(lmodvol5)['SwapRate'])
abline(lm(d15~m15), col=2)

plot(m16,d16,xlab="HV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for HV
abline(0,coef(lmodvol5)['HV'])
abline(lm(d16~m16), col=2)

plot(m17,d17,xlab="VIX residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for VIX
abline(0,coef(lmodvol5)['VIX'])
abline(lm(d17~m17), col=2)

plot(m18,d18,xlab="Curve residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Curve
abline(0,coef(lmodvol5)['Curve'])
abline(lm(d18~m18), col=2)

plot(m19,d19,xlab="SPXHV residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for SPXHV
abline(0,coef(lmodvol5)['SPXHV'])
abline(lm(d19~m19), col=2)

plot(m20,d20,xlab="CDXHY residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for CDXHY
abline(0,coef(lmodvol5)['CDXHY'])
abline(lm(d20~m20), col=2)

plot(m21,d21,xlab="Spread residuals", ylab="df residuals", main="partial regression plot")
#Line through (0,0) with slope beta for Spread
abline(0,coef(lmodvol5)['Spread'])
abline(lm(d21~m21), col=2)


#Same coefficients
coef(lm(d15~m15))
coef(lm(d16~m16))
coef(lm(d17~m17))
coef(lm(d18~m18))
coef(lm(d19~m19))
coef(lm(d20~m20))
coef(lm(d21~m21))
coef(lmodvol5)


#Run Partial Residual Plots for all 3 of the models, for every variable
#Simple Model
par(mfrow=c(2,4))
termplot(lmodvol2,partial.resid=T,terms=1,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=2,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=3,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=4,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=5,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=6,col.res="black")
termplot(lmodvol2,partial.resid=T,terms=7,col.res="black")

par(mfrow=c(2,4))
termplot(lmodvol3,partial.resid=T,terms=1,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=2,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=3,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=4,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=5,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=6,col.res="black")
termplot(lmodvol3,partial.resid=T,terms=7,col.res="black")

par(mfrow=c(2,4))
termplot(lmodvol5,partial.resid=T,terms=1,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=2,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=3,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=4,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=5,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=6,col.res="black")
termplot(lmodvol5,partial.resid=T,terms=7,col.res="black")










###################################################  Prediction  ######################################################
pred=t(c(1,1.8315,42.018,12.7,.13,329.6))%*%coef(lmodvol)
pred=pred
pred


#Pull data in from excel files to create multiple data frames ordered by date
df3_swapIV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(2:3))
df3_swapRates <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(5:6))
df3_swapHV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(5,7))
df3_VIX <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(9:10))
df3_curve <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(12:13))
df3_SPX <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(15:16))
df3_SPXHV <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(15,17))
df3_CDXHY <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(19:20))
df3_Spreads <- read.xlsx("C:/Users/pjant/Drew/DATA 503/Dataset for Final Project.xlsm",sheet=2, detectDates=TRUE, startRow=2,cols=c(22:23))

#Combine these into one dataframe based on date
df3<-Reduce(function(x,y) merge(x,y,all=TRUE),list(df3_swapIV,df3_swapRates,df3_swapHV,df3_VIX,df3_curve,df3_SPX,
                                                   df3_SPXHV,df3_CDXHY,df3_Spreads))

df3$SWAP10.HV<-df3$SWAP10.HV*100

df3<-na.omit(df3)

#Move the dates from being a column of data, to being the actual rownames
rownames(df3)<-df3[,1]
df3<-df3[-1]

#Rename the columns of the dataframe
colnames(df3) <- c("IV", "SwapRate", "HV", "VIX", "Curve", "SPX", "SPXHV", "CDXHY", "Spread")
df3$IV=df3$IV/df3$SwapRate

pred2=as.data.frame(cbind(predict(lmodvol2,new=df3, interval="prediction"),df3$IV))
colnames(pred2)[4]="actual"

pred3=as.data.frame(cbind(predict(lmodvol3,new=df3, interval="prediction"),df3$IV))
colnames(pred3)[4]="actual"

pred5=as.data.frame(cbind(predict(lmodvol5,new=df3, interval="prediction"),df3$IV))
colnames(pred5)[4]="actual"

predictions=as.data.frame(cbind(as.Date(rownames(pred2),format="%Y-%m-%d"),round(pred2$fit,3),round(pred3$fit,3),
                                round(pred5$fit,3),round(df3$IV,3)))
#rownames(predictions)=rownames(pred2)
colnames(predictions)=c("Dates","SimpleModel","TransformedModel", "SinglePolyModel","Actual")
predictions[,1]=as.Date(predictions[,1],origin="1970-01-01")
predictions[,3]=1/(predictions$TransformedModel^4)
predictions


predictions_long=melt(predictions,id="Dates")
colnames(predictions_long)[2]="Model"

ggplot(data=predictions_long,aes(x=Dates,y=value,color=Model))+
  geom_line(aes(linetype=Model,color=Model,size=Model))+
  ylim(0,300)+
  scale_x_date(date_breaks="1 week")+
#  theme_bw()+
#  theme_stata()+
#  scale_color_stata("s1color")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(x="Dates\n",y="IV Values\n")+
  scale_color_manual(values=c("red","blue","dark green","black"))+
  scale_linetype_manual(values=c("dashed","dashed","dashed","solid"))+
  scale_size_manual(values=c(1,1,1,1))+
  ggtitle("Actual IV Values v Predicted IV Values\n")+
  theme(plot.title=element_text(size=24,hjust=0.5),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        legend.title=element_text(size=16),
        legend.text=element_text(size=10))
#  geom_point(aes(x=as.Date("2019-11-20"),y=40),data=predictions_long,size=25,shape=1,color="grey24",stroke=1)+
#  geom_point(aes(x=as.Date("2020-04-03"),y=42.5),data=predictions_long,size=25,shape=1,color="grey24",stroke=1)


################################## Check the r-squared's of the predctors as they are added ###########################

# Other tested models
lmodvol_t1<-lm(IV~SwapRate,data=df2)
summary(lmodvol_t1)

lmodvol_t2<-lm(IV~SwapRate+HV,data=df2)
summary(lmodvol_t2)

lmodvol_t3<-lm(IV~SwapRate+HV+VIX,data=df2)
summary(lmodvol_t3)

lmodvol_t4<-lm(IV~SwapRate+HV+VIX+Curve,data=df2)
summary(lmodvol_t4)

lmodvol_t5<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV,data=df2)
summary(lmodvol_t5)

lmodvol_t6<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY,data=df2)
summary(lmodvol_t6)

lmodvol_t7<-lm(IV~SwapRate+HV+VIX+Curve+SPXHV+CDXHY+Spread,data=df2)
summary(lmodvol_t7)
