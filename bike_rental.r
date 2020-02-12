#get Working directory
getwd()

setwd("C:/Users/jhemmann/Desktop/edwisor-project-2/Project_bike/")

#Install required packages 

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"dplyr","plyr","ggplot2","data.table","GGally","caret")

lapply(x, require, character.only = TRUE)

#loading the Data 
bike_df = read.csv('day.csv',header = T)

#viewing the first data 
head(bike_df)

#summarizing the data 
summary(bike_df)

#structure of data 
str(bike_df)

set.seed(111)

univariant <- function(num){
  ggplot(bike_df) +
    geom_histogram(aes(x=num,y=..density..),fill='grey') +
    geom_density(aes(x=num,y=..density..))
}

# analyze the distribution of  target variable 'cnt'
univariant(bike_df$cnt)
#'cnt' is normally disturubuted 

# analyse the distrubution of  independence variable 'temp'
univariant(bike_df$temp)

# analyse the distrubution of  independence variable 'atemp'
univariant(bike_df$atemp)

# analyse the distrubution of  independence variable 'hum'
univariant(bike_df$hum)

# analyse the distrubution of  independence variable 'windspeed'
univariant(bike_df$windspeed)

# analyse the distrubution of  independence variable 'casual'
univariant(bike_df$casual)

# analyse the distrubution of  independence variable 'registered'
univariant(bike_df$registered)

#visulizating the categorical varibale
ggplot(bike_df,aes(x = as.factor(mnth), y = cnt),fill='grey') +
  stat_summary(fun.y="mean", geom="bar")

#visulizatating the 'holiday'
ggplot(bike_df) + 
  geom_bar(aes(x=holiday),fill='red')

#most cycle rental happends only on holiday

#visulizatating the 'weekday'
ggplot(bike_df) + 
  geom_bar(aes(x=weekday),fill='red')

#cycle rental count is almost same on modt weekday

#visulizatating the 'weathersit'
ggplot(bike_df) + 
  geom_bar(aes(x=weathersit),fill='red')

# count  is more when  whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"

#####################################################################################
#bivariate  relationship between numeric variables

#check the relationship between 'temp' and 'atemp' variable

ggplot(bike_df, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#from this graph we can see that temp and atemp us strongly releated

#check the relationship between 'temp' and 'hum' variable

ggplot(bike_df, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'temp' and 'windspeed' variable

ggplot(bike_df, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed

#checking the relationship b/w the numerical varibale 
ggpairs(bike_df[,c('atemp','temp','hum','windspeed','cnt')])

###############################################################################################
#Relationship b/w categorical varibale 


#relationship b/w season and holiday
season_holiday_table = table(bike_df$season,bike_df$holiday)
barplot(season_holiday_table)
season_holiday_table
#we can see that in the hoilday the value is almost same.

#relationship b/w season and weekday
season_weekday_table = table(bike_df$season,bike_df$weekday)
barplot(season_weekday_table)
season_weekday_table
#from the graph and table we can see that value on all the weekday are almost same.

#releationship b/w season and weathersit
season_wethersit_table = table(bike_df$season,bike_df$weathersit)
barplot(season_wethersit_table)
season_wethersit_table
#from graph and table we can see that count of weather type 1 is higest

#relationship b/w holiday and weathersit
holiday_weathersit_table = table(bike_df$weathersit,bike_df$holiday)
barplot(holiday_weathersit_table)
holiday_weathersit_table
#from graph we can see that weather type 1 and holiday 0's count is highest

##################################################################################################
#missing value analysis

missing_val = data.frame(apply(bike_df, 2, function(x){sum(is.na(x))}))
missing_val

#there is no missing value present in this data set

##################################################################################################
#outlier analysis 

#detect outlier in 'actual','registered' and 'cnt' varibale
ggplot(data = bike_df,aes(x="",y = casual)) +
  geom_boxplot()
#there are some outliers present here.

ggplot(data = bike_df,aes(x="",y=registered)) +
  geom_boxplot()
#no outliers here

ggplot(data = bike_df,aes(x="",y=cnt)) +
  geom_boxplot()
#no ouliers here also

##################################################################################################
#treating the outliers

ggplot(bike_df,aes(x=casual,y=cnt)) +
  geom_point() +
  geom_smooth()

bike_df_out <- bike_df

#removing the outliers using the boxplot
val = bike_df_out$casual[bike_df_out$casual %in% boxplot.stats(bike_df_out$casual)$out]

bike_df_out = bike_df_out[which(!bike_df_out$casual %in% val),]

ggplot(bike_df_out,aes(x=casual,y=cnt)) +
  geom_point() +
  geom_smooth()

# boxplot for  casual variable
ggplot(data = bike_df_out, aes(x = "", y = casual)) + 
  geom_boxplot() 

cor(bike_df$casual,bike_df$cnt)
#0.6728044
cor(bike_df_out$casual,bike_df_out$cnt) 
#0.6460021

#since we removed some of the outliers so the correlation is reduced after removing the outliers 

###############################################################################################
#Feature selection

library(corrgram)

# verify correleation between   Numeric variable

corrgram(bike_df[,c('temp','atemp','hum','windspeed','cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#from the graph we can see that temp and atemp are highly coreleated 
#hum and cnt are negatively correleated 

bike_df_feature = subset(bike_df,select=-c(atemp,hum))

################################################################################################
#Normalization

col = c("casual","registered")

for (i in col) {
  bike_df_feature[,i] = (bike_df_feature[,i] - min(bike_df_feature[,i]))/
    (max(bike_df_feature[,i] - min(bike_df_feature[,i])))
}

#############################################################################################
#model development

feature_train_col = c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")

train.index = createDataPartition(bike_df_feature$cnt, p = .80, list = FALSE)
train = bike_df_feature[ train.index,]
test  = bike_df_feature[-train.index,]

train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

test_features

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}


#Evaluate  Model using RMSE
RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}
#################################################################################################
train_feature_lr = train[,c("season","yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
test_features_lr = test[,c("season","yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

#liner regression
lr = lm(cnt ~ .,data = train_feature_lr)
``
summary(lr)


#predict
predict_lr = predict(lr,test_features_lr[,-12])

MAPE(test_features_lr[,12], predict_lr)
#error rate = 1.879512e-17
#accuracy = 99.9+ 

RMSE(test_features_lr[,12], predict_lr)
#RMSE = 2.066962e-13

################################################################################################
#Random Forest
RF = randomForest(cnt ~ .,data = train_feature)

RF

plot(RF)

#predict
predict_rf = predict(RF,test_features[,-12])

MAPE(test_features[,12],predict_rf)
#Error rate = 0.045
#Accuracy = 95.5

RMSE(test_features[,12],predict_rf)
#RMSE = 247.0241

###############################################################################################
#Decision tree

dt = rpart(cnt ~ .,data = train_feature,method = "anova")

#predicting 
predict_dt = predict(dt,test_features[,-12])

print(dt)

#plotting
par(cex=0.8)
plot(dt) 
text(dt)

#Evaluating the DT

MAPE(test_features[,12], predict_dt)
#Error rate = 0.1234
#accuracy = 87.64

RMSE(test_features[,12], predict_dt)
#RMSE = 614.3601


