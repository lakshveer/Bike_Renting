#******************Model Building*********************

#Library
library(gbm) #For gbm model
library(pdp) #For partial dependency plot
library(lime) #For more explanations
library(plotmo) #For residual plots


d = daily_data
daily_data = d


#Variables to be droped
drop_variables = c("casual", "registered", "date", "temp", "instant")
#Dropping variables
daily_data = daily_data[ , !(names(daily_data) %in% drop_variables)]

#setting seed
set.seed(123)

#Dividing the data into train and test
sample = sample.int(n = nrow(daily_data), size = floor(.80*nrow(daily_data)), replace = F)
train = daily_data[sample, ]
test  = daily_data[-sample, ]


#*****************************Gradient Boosting model************************************

#Fitting the model
gbm.fit = gbm(
  formula = train$total_count ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 100,
  interaction.depth = 5,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  

#Prining the fitted model
print(gbm.fit)

#Cheking the min cross validation error on the train data
sqrt(min(gbm.fit$cv.error))  #0.07861281

#plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv") 

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit$cv.error) #86




#Let's tune to the model to see if we can reduce our rmse more
#Tuning the model
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(3, 5, 7),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = train$total_count ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

#Arrange model parameter combination based on min_RMSE
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


"After tuning the parameter looks like our initial model works better as tuned parameters gives us more RMSE then our inital model
with more no of trees. We will stick to our inital model with 100 trees."




#**************************************************Visualizing the Model****************************************

#Relative Influence plot
summary(
  gbm.fit, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)


#Partial dependency plot
"The PDP plot below displays the average change in predicted total count of rental bikes as we vary atemp which is feeling 
temprature while holding all other variables constant"

gbm.fit %>%
  partial(pred.var = "atemp", n.trees = gbm.fit$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = train) 



#Using LIME for understanding why a prediction resulted in a given value for a single observation
#Defining model type and prediction method
model_type.gbm <- function(x, ...) {
  return("regression")
}

predict_model.gbm <- function(x, newdata, ...) {
  pred <- predict(x, newdata, n.trees = x$n.trees)
  return(as.data.frame(pred))
}

#Taking the first five test observations
local_obs <- test[1:5, ]

#Checking which predictors are supporting and contradicting and by how much in explaing the test observations
explainer <- lime(train, gbm.fit)
explanation <- explain(local_obs, explainer, n_features = 5)
plot_features(explanation)

#Plot Explanations
plot_explanations(explanation)



#Plotting residuals
plotres(gbm.fit)



#*******************************************************Prediction and Model Evaluation**********************************
#Predicting the values
pred = predict(gbm.fit, n.trees = gbm.fit$n.trees, test)

#Model Evaluation 
root_mean_sq_error = caret::RMSE(pred, test$total_count) #0.07758642

mean_absolute_error = caret::MAE(pred, test$total_count)  #0.05454643

mean_square_error = (root_mean_sq_error)^2  #0.006019652

#Denormalizing predicted total rental bike count
pred_denormalized = denormalizeData(pred, getNormParameters(daily_data$total_count))

#R2 on train and test
#Rsquare on train data
CV_RSq = (cor(gbm.fit$cv.fitted, train$total_count))^2 #0.8773097

#Rsquare on test data
Train_RSq = (cor(gbm.fit$fit, train$total_count))^2 #0.9467672
