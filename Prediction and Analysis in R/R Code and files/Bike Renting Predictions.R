getwd()

#Libraries
x = c("ggplot2", "DMwR", "data.table", "corrgram", "ggcorrplot", "gbm", "gridExtra", "RSNNS", "e1071", "Metrics",
      "plyr", "pdp", "gbm", "lime", 'plotmo', 'RColorBrewer', 'extrafont', 'ggpubr')

lapply(x, require, character.only = TRUE)

"
library(ggplot2) #ggplots
library(DMwR) #for knn imputation
library(data.table) #
library(corrgram) #For correlation matrix
library(ggcorrplot) #For correlation plot
library(gbm)
library(gridExtra)
library(RSNNS) #For normalization and denormalization
library(e1071) #For skewness value
library(Metrics) #MSE
library(plyr) #For aggregation
library(pdp) #For partial dependency plot
library(lime) #For more explanations
library(plotmo) #For residual plots
library(RColorBrewer)
library(extrafont)
library(ggpubr) #for ggscatter and stat_cor
"
set.seed(123)

#importing dataset
daily_data = read.csv("day.csv", header = T, as.is = T)

#Checking the data type and structure
head(daily_data); str(daily_data);
names(daily_data); summary(daily_data);


'As there is inconsitency in the variables names as some are abbreviated and some are not, we will rename them to proper format
so it becomes more readable'

setnames(daily_data, old = c("dteday", "yr", "mnth", "weathersit", "cnt", 'hum') , new = c('date', 'year', 'month', 'weather_type', 'total_count', "humidity"))


"As we can see there categorical variables are in the form of 'int' data type, we will convert it to factor variables"
datatype_conversion = function(variables, data_type)
{
  daily_data[variables] = lapply(daily_data[variables], data_type)
  return(daily_data[variables])
}

factor_variables = c("season", "year", "month", "holiday", "weekday", "workingday", "weather_type")

#invoking the datatype_conversion function
daily_data[factor_variables] = datatype_conversion(factor_variables, factor)





#*****************************************Completeness of Data************************************************************

#Creating a data frame with missing value count
missing_val = data.frame(apply(daily_data,2,function(x){sum(is.na(x))}))
#There is no missing value in the data set.






#************************************************Outlier Analysis********************************************************

numeric_index = sapply(daily_data,is.numeric) #selecting only numeric
numeric_data = daily_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "total_count"), data = subset(daily_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "#CCFFFF" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5))+
           labs(y=cnames[i],x="Total Rented Bikes")+
           ggtitle(paste("Total rented bike box plot for",cnames[i])))
}

#Plotting together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4, gn5,gn6,gn7,gn8, ncol=2)


"As we can see casual contains outlier and total_count does not so we are not going to remove outliers in the casual variables
as we will be going to remove this variable in the model generation. If we impute the casual variable then we need to adjust
total_count as well as total_count is the summation of casual and registered otherwise there will be some conflict and it's better
not to change anything in our target variable.
"
#Removing casual fro the cnames list
cnames = cnames[cnames != "casual"]

#As our data is less we will impute the outliers treating them as missing value with the help of KnnImputation
for(i in cnames)
{
  cnames
  val = daily_data[,i][daily_data[,i] %in% boxplot.stats(daily_data[,i])$out]
  print(length(val))
  daily_data[,i][daily_data[,i] %in% val] = NA
}

#As knnimputation takes only numeric variables into consideration, converting all the 

#As knn imputation only takes feature with numbers into consideration, we need to drop 'dteday' variable and place it back after imputation
x = daily_data['date']
daily_data$date = NULL

daily_data = knnImputation(daily_data, k = 3)

daily_data = cbind(daily_data, x)

#Setting the date variable to it's original position
daily_data = daily_data[,c(1,16, 2:15)]


#Adding casual back into the list of cnames as we will we using it in EDA
cnames = c(cnames, "casual")




#******************************************************Correlation***********************************************************
#Using ggheat map

#Correlation between continuous variables
corrdata = daily_data[, numeric_index]
p.mat = cor_pmat(corrdata)
cormat = round(cor(corrdata),2)
ggcorrplot(cormat, hc.order = TRUE, type = "lower", lab = TRUE)


#Correlation between categorical variables
factor_data = daily_data[,factor_variables]
fcnames = colnames(factor_data)

for (i in 1:length(fcnames))
{
  for (j in 2:length(fcnames))
  {
    print(names(factor_data)[i])
    print(names(factor_data)[j])
  print(chisq.test(table(factor_data[,i], factor_data[,j])))
  }
}


#Finding relationship between categorical variable and target variable(Continuous)
#Using Anova

for(i in 1:length(fcnames))
{
 assign(paste0("anova_", i), aov(daily_data$total_count ~ factor_data[,i]))
# anova = aov(factor_data[,i] ~ daily_data$total_count))
}

summary(anova_7)





#***************************************EDA********************************************************************

#**************Univaraite Analysis: Continuous: Distribution of Continuous variables*********************************

for (i in 1:length(cnames))
  {
     assign(paste0("density", i), ggplot(aes_string(x = cnames[i]), data = numeric_data) +
              geom_density(color="darkblue", fill="#CCFFFF") +
            ggtitle(paste("Density plot for", cnames[i])) +
              theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5)) + geom_vline(aes_string(xintercept = mean(numeric_data[,i])), color="blue", linetype="dashed", size=1))
  }
 
grid.arrange(density1, density2, density3, density4, density5, density6, density7, density8,ncol=2)


#*********Categorical Variable****************

for( i in 1:length(fcnames))
{
  assign(paste0("bar_univarite_", i), ggplot(aes_string(x = fcnames[i]), data = factor_data) +
           geom_bar(stat = 'count', position = 'dodge', fill = "#CCFFFF", col = 'black') + 
           geom_label(stat = 'count', aes(label = ..count..),  col = 'black') +
         ggtitle(paste("Univarite bar plot for", fcnames[i])) + theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) )
}

grid.arrange(bar_univarite_1, bar_univarite_2, bar_univarite_3, bar_univarite_4, bar_univarite_5, bar_univarite_6, bar_univarite_7,ncol=2)

grid.arrange(bar_univarite_4, bar_univarite_6,bar_univarite_7)



#*********Bi-variate distribution****Categorical and target*************************

#stat_boxplot(geom = "errorbar", width = 0.5) 
#Imp we cannot use stat_count with a y aesthetic
for (i in 1:length(fcnames))
{
  assign(paste0("bi_var_cat_target_",i), ggplot(aes_string((x= fcnames[i]), y = daily_data$total_count, fill = fcnames[i]), data = daily_data)+ 
           geom_boxplot(outlier.colour="red", outlier.shape=18, outlier.size=1, notch=FALSE) +
           stat_summary(fun.y="mean", geom="point", size=1,position=position_dodge(width=0.75), color="red") +
           theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
           labs(x=fcnames[i], y="total_count") +
           scale_fill_brewer(palette="Set3", type = "qual") +
           ggtitle(paste("Total rented bike box plot for ",fcnames[i])))
}

grid.arrange(bi_var_cat_target_1, bi_var_cat_target_2, bi_var_cat_target_3, bi_var_cat_target_4, bi_var_cat_target_5, bi_var_cat_target_6, bi_var_cat_target_7, ncol=2)


#********Bi-Variate Analysis***Continuous and target**************

for (i in 1:length(cnames))
{
  assign(paste0("bi_var_conti_target_",i), ggscatter(daily_data, x = cnames[i], y = "total_count", color = "#3399FF",
                                                     add = "reg.line", conf.int = TRUE,     
                                                     add.params = list(color = "black", fill = "#CCFFFF"),
                              ggtheme = theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5))) + 
  ggtitle(paste("Total rented bike point distribution with ", cnames[i])) + stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top"))
}


grid.arrange(bi_var_conti_target_1, bi_var_conti_target_2, bi_var_conti_target_3, bi_var_conti_target_4, bi_var_conti_target_5, bi_var_conti_target_6, bi_var_conti_target_7, ncol=2)


point1 = ggplot(daily_data, aes(x=atemp, y=total_count, color=weather_type)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


point2 =ggplot(daily_data, aes(x=windspeed, y=total_count, color=weather_type)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)

point3 = ggplot(daily_data, aes(x=humidity, y=total_count, color= weather_type)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)

point4 = ggplot(daily_data, aes(x=atemp, y=total_count, color=season)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)


 point5 = ggplot(daily_data, aes(x=atemp, y= humidity)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE)

 
 grid.arrange(point1, point2, point3, point4, point5, ncol = 2)
 
 
 
facet1 = ggplot(daily_data, aes(x= workingday, y = total_count, fill = workingday))+ 
  geom_boxplot() + theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
  scale_fill_brewer(palette="Set3", type = "qual") +
  ggtitle("Total rented bike box plot for working data based on weather type") +
  facet_wrap(.~daily_data$weather_type)

facet2 = ggplot(daily_data, aes(x= weekday, y = total_count, fill = weekday))+ 
  geom_boxplot() + 
  theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
  scale_fill_brewer(palette="Set3", type = "qual") +
  ggtitle("Total rented bike box plot for weekday based on weather type") +
  facet_wrap(.~daily_data$weather_type)

facet3 = ggplot(daily_data, aes(x= season, y = total_count, fill = season))+ 
  geom_boxplot() + 
  theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
  scale_fill_brewer(palette="Set3", type = "qual") +
  ggtitle("Total rented bike box plot for season based on weather type ") +
  facet_wrap(.~daily_data$weather_type)


facet4 = ggplot(daily_data, aes(x= month, y = total_count, fill = month))+ 
  geom_boxplot() + 
  theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
  scale_fill_brewer(palette="Set3", type = "qual") +
  ggtitle("Total rented bike box plot for month based on weather type ") +
  facet_wrap(.~daily_data$weather_type)

facet5 = ggplot(daily_data, aes(x= year, y = total_count, fill = year))+ 
  geom_boxplot() +
  theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) +
  scale_fill_brewer(palette="Set3", type = "qual") +
  ggtitle("Total rented bike box plot for year based on weather type") +
  facet_wrap(.~daily_data$weather_type)

grid.arrange(facet1, facet2, facet3, facet4, facet5, ncol = 2)




#Aggregating the results
aggre.weather.working =  ddply(daily_data, c( "weather_type", "workingday"), function(x) colSums(x[c("total_count", "registered", "casual")]))

aggre.weather.weekday = ddply(daily_data, c( "weather_type", "weekday"), function(x) colSums(x[c("total_count", "registered", "casual")]))

aggre.weather.season = ddply(daily_data, c( "weather_type", "season"), function(x) colSums(x[c("total_count", "registered", "casual")]))

aggre.weather.month = ddply(daily_data, c( "weather_type", "month"), function(x) colSums(x[c("total_count", "registered", "casual")]))

aggre.weather_year = ddply(daily_data, c( "weather_type", "year"), function(x) colSums(x[c("total_count", "registered", "casual")]))




#********************************************Feature Scaling*******************************************************

#All the numerica features are in the range(0,1) except casual, registered and total count.
#The area of our study lies only for total count and we won't be taking casual and registered features in our model generation so let's
#check out target variable i.e total count

#Lets first check the skewness in the total count

#Getting the skewness value
skew_value_total_count = skewness(daily_data$total_count) 
skew_value_total_count
#Skewness value is -0.047 which is pretty close to 0, so log transformation doesn't seem to be a good appraoch here, we will be
#normalizing our data in the range(0,1)
daily_data$total_count = normalizeData(daily_data$total_count, type = "0_1")
