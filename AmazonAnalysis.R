# Amazon

# setwd("C:/Users/farri/OneDrive/Documents/PA/AmazonEmployeeAccess")

# LIBRARIES=====================================================================

library(tidyverse)
library(vroom)
library(tidymodels)
library(discrim)
library(glmnet)
library(rpart)
library(ranger)
library(stacks)
library(recipes)
library(embed) 
library(naivebayes)
library(kknn)
library(themis) # for smote

# READ IN THE DATA==============================================================

test <- vroom("test.csv")
train <- vroom("train.csv")
sampleSubmission <- vroom("sampleSubmission.csv")

train$ACTION <- as.factor(train$ACTION)



Recipe1 <- recipe(ACTION ~ ., data = train) %>%
step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_factor_predictors(), threshold = .01) %>% # combines categorical values that occur <5% into an "other" value
  step_dummy(all_nominal_predictors()) # %>% # dummy variable encoding
  # step_lencode_mixed(all_nominal_predictors(), outcome = vars(target_var)) #target encoding
# also step_lencode_glm() and step_lencode_bayes()

prep <- prep(Recipe1)
bake(prep, new_data = train)


Log_Model <- logistic_reg() %>% #Type of model
  set_engine("glm")

Log_WF <- workflow() %>%
add_recipe(Recipe1) %>%
add_model(Log_Model) %>%
fit(data = train) # Fit the workflow

predictions <- predict(Log_WF,
                              new_data=test,
                              type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)

  
vroom_write(x=predictions, file="./Log_Prediction.csv", delim=",")

# PENALIZED ====================================================================

# all_nominal_predictors()

Recipe2 <- recipe(ACTION ~ ., data = train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  # step_zv(all_predictors()) %>% 
  #step_other(all_factor_predictors(), threshold = .01)  %>% # combines categorical values that occur <5% into an "other" value
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) #target encoding

prep2 <- prep(Recipe2)
view(bake(prep2, new_data = train))

pen_mod <- logistic_reg(mixture=tune(), penalty=tune()) %>% #Type of model
  set_engine("glmnet")

pen_wf <- workflow() %>%
add_recipe(Recipe2) %>%
add_model(pen_mod)

## Grid of values to tune over
pen_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(train, v = 5, repeats=1)

## Run the CV
CV_results <- pen_wf %>%
tune_grid(resamples=folds,
          grid=pen_grid,
          metrics=metric_set(roc_auc)) #, f_meas, sens, recall, spec,
                             #precision, accuracy)) #Or leave metrics NULL

# Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("roc_auc")

## Finalize the Workflow & fit it
final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=myDataSet)

## Predict
final_wf %>%
predict(new_data = myNewData, type=)

# Random Forest=================================================================



## Create a workflow with model & recipe
Recipe2 <- recipe(ACTION ~ ., data = train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  # step_zv(all_predictors()) %>% 
  #step_other(all_factor_predictors(), threshold = .01)  %>% # combines categorical values that occur <5% into an "other" value
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) #target encoding

## Set up K-fold CV

forest_model <- rand_forest(mtry= tune(),
                            min_n=tune(),
                            trees=1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("classification")

grid <- grid_regular(mtry(range = c(1,9)),
                            min_n(),
                            levels = 2) ## L^2 total tuning possibilities

forestwf <- workflow() %>%
  add_recipe(Recipe2) %>%
  add_model(forest_model) %>%
  fit(data=train) 

## Finalize workflow and predict

folds <- vfold_cv(train, v = 5, repeats=1)

CV_results <- forestwf %>%
  tune_grid(resamples=folds,
            grid=grid,
            metrics=metric_set(roc_auc)) #Or leave metrics NULL

## Find best tuning parameters

bestTune <- CV_results %>%
  select_best("roc_auc")

forest_model2 <- rand_forest(mtry = 1,
                            min_n=2,
                            trees=1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("classification")

forestwf2 <- workflow() %>%
  add_recipe(Recipe2) %>%
  add_model(forest_model2) %>%
  fit(data=train) 

predictions2 <- predict(forestwf2,
                       new_data=test,
                       type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictions2, file="./Log_Prediction2.csv", delim=",")
















# BAYES ========================================================================


## nb model
nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
set_mode("classification") %>%
set_engine("naivebayes") # install discrim library for the naivebayes eng

nb_model <- naive_Bayes(Laplace=2, smoothness=2) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_wf <- workflow() %>%
add_recipe(Recipe2) %>%
add_model(nb_model)%>%
  fit(data=train) 

grid <- grid_regular(Laplace(),
                     smoothness(),
                     levels = 2) ## L^2 total tuning possibilities

## Tune smoothness and Laplace here
folds <- vfold_cv(train, v = 5, repeats=1)

CV_results <- nb_wf %>%
  tune_grid(resamples=folds,
            grid=grid,
            metrics=metric_set(roc_auc)) #Or leave metrics NULL

## Find best tuning parameters

bestTune <- CV_results %>%
  select_best("roc_auc")


## Predict
predict(nb_wf, new_data=myNewData, type=)

predictions3 <- predict(nb_wf,
                        new_data=test,
                        type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictions3, file="./Prediction3.csv", delim=",")

# KNN ==========================================================================

Recipe3 <- recipe(ACTION ~ ., data = train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_factor_predictors(), threshold = .01) %>% # combines categorical values that occur <5% into an "other" value
  step_dummy(all_nominal_predictors())  %>% # dummy variable encoding
  step_normalize(all_numeric())
# step_lencode_mixed(all_nominal_predictors(), outcome = vars(target_var)) #target encoding

## knn model
knn_model <- nearest_neighbor(neighbors=20) %>% # set or tune
  set_mode("classification") %>%
  set_engine("kknn") 

knn_wf <- workflow() %>%
add_recipe(Recipe3) %>%
add_model(knn_model)%>%
  fit(data=train) 

## Fit or Tune Model HERE

predictions4 <- predict(knn_wf,
                        new_data=test,
                        type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictions4, file="./Prediction4.csv", delim=",")

# PCA ==========================================================================

Recipe4 <- recipe(ACTION ~ ., data = train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_factor_predictors(), threshold = .01) %>% # combines categorical values that occur <5% into an "other" value
  step_dummy(all_nominal_predictors())  %>% # dummy variable encoding
  step_normalize(all_numeric())# %>%
  step_pca(all_predictors(), threshold=.95)
  
Recipe4 <- recipe(ACTION ~ ., data = train) %>%
    step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
    step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>% 
    step_normalize(all_numeric())# %>%
    step_pca(all_predictors(), threshold=.90)



## PCA model
PCA_model <- naive_Bayes(Laplace=2, smoothness=2.5) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")


PCA_wf <- workflow() %>%
  add_recipe(Recipe4) %>%
  add_model(PCA_model)%>%
  fit(data=train) 
 

## Fit or Tune Model HERE

predictions5 <- predict(PCA_wf,
                        new_data=test,
                        type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictions5, file="./Prediction5.csv", delim=",")



# SVM ==========================================================================

svmPoly <- svm_poly(degree=tune(), cost=tune()) %>% # set or tune
  set_mode("classification") %>%
set_engine("kernlab")

svmLinear <- svm_linear(cost=tune()) %>% # set or tune
  set_mode("classification") %>%
set_engine("kernlab")


svmRadial <- svm_rbf(rbf_sigma=2, cost=4) %>% # set or tune
  set_mode("classification") %>%
  set_engine("kernlab")

svm_wf <- workflow() %>%
  add_recipe(Recipe2) %>%
  add_model(svmRadial)%>%
  fit(data=train) 

grid <- grid_regular(rbf_sigma(),
                     cost(),
                     levels = 3) ## L^2 total tuning possibilities

## Tune smoothness and Laplace here
folds <- vfold_cv(train, v = 4, repeats=1)

CV_results <- svm_wf %>%
  tune_grid(resamples=folds,
            grid=grid,
            metrics=metric_set(mse)) #Or leave metrics NULL

## Find best tuning parameters

bestTune <- CV_results %>%
  select_best("roc_auc")


## Predict
predict(nb_wf, new_data=myNewData, type=)

predictions3 <- predict(svm_wf,
                        new_data=test,
                        type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictions3, file="./Prediction6.csv", delim=",")

# Smote ========================================================================

Smote_Recipe <- recipe(ACTION ~ ., data = train) %>%
step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%  #target encoding
#Everything numeric for SMOTE so encode it here
step_smote(all_outcomes(), neighbors=8)
# also step_upsample() and step_downsample()

# apply the recipe to your data
prepped_recipe <- prep(my_recipe)
baked <- bake(prep, new_data = )


## Set up K-fold CV

Smote_Prep <- rand_forest(mtry= tune(),
                            min_n=tune(),
                            trees=1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("classification")

grid <- grid_regular(mtry(range = c(1,9)),
                     min_n(),
                     levels = 5) ## L^2 total tuning possibilities

wf_prep <- workflow() %>%
  add_recipe(Smote_Recipe) %>%
  add_model(Smote_Prep) %>%
  fit(data=train) 

## Finalize workflow and predict

folds <- vfold_cv(train, v = 5, repeats=1)

CV_results <- wf_prep %>%
  tune_grid(resamples=folds,
            grid=grid,
            metrics=metric_set(roc_auc)) #Or leave metrics NULL

## Find best tuning parameters

bestTune <- CV_results %>%
  select_best("roc_auc")

Smote_Model <- rand_forest(mtry = 1,
                             min_n=10,
                             trees=1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("classification")

SmoteWF <- workflow() %>%
  add_recipe(Smote_Recipe) %>%
  add_model(Smote_Model) %>%
  fit(data=train) 

predictionsSmote <- predict(SmoteWF,
                        new_data=test,
                        type="prob") %>%  # "class" or "prob" (see doc) 
  bind_cols(., sampleSubmission) %>% 
  select(Id, .pred_1) %>% 
  rename(Action=.pred_1)


vroom_write(x=predictionsSmote, file="./Log_PredictionSmote.csv", delim=",")


















