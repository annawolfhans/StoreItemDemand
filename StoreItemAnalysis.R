library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)
library(lubridate)
library(embed)


storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

#### EDA with ACF Plots ####

# 
# # We want to create a ACF plot that shows the differences
# # in the autocorrelation structure for the different items & upload the image to LS
# 
# # take out rows where item and store ==1
# acf11 <- storeTrain %>%
#   filter(store==1, item==1)
# acf44 <- storeTrain %>%
#   filter(store==4,item==4)
# acf22<- storeTrain %>%
#   filter(store==2,item==2)
# acf33 <- storeTrain %>%
#   filter(store==3,item==3)
# 
# acf11 %>%
#   pull(sales) %>%
#   forecast::ggAcf(.)
# acf22 %>%
#   pull(sales) %>%
#   forecast::ggAcf(.)
# acf33 %>%
#   pull(sales) %>%
#   forecast::ggAcf(.)
# acf44 %>%
#   pull(sales) %>%
#   forecast::ggAcf(.)
# 
# par(mfrow=c(2,2))
# 
# plot1 <- acf11 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max=2*365)  + ggtitle("Store=1,Item=1,Lag=2years")
# 
# 
# plot2 <- acf22 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=2,Item=2,Lag=2years")
# 
# 
# plot3 <- acf33 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=3,Item=3,Lag=2years")
# 
# 
# plot4 <- acf44 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=4,Item=4,Lag=2years")
# 
# 
# grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
# 
# # Save the arranged plot using ggsave
# ggsave("output_plot1.png", width = 10, height = 8)


# nStores <- max(storeTrain$store)
# nItems <- max(storeTrain$item)
# for (s in 1:nStores){
#    for (i in 1:nItems){
#      storeItemTrain <- storeTrain %>%
#        filter(store==s, item==i)
#      storeItemTest <- test %>%
#        filter(store==s,item==i)
#    } 
#   
#   if(s==1 &i==1){
#     all_preds <- preds
#   }
#   else {
#     all_preds<- bind_rows(all_preds,preds)
#       }
#   }
# }

#### Random Forests #### 
storeTrain <- storeTrain %>%
  filter(store==3, item==6)

store_recipe <- recipe(sales ~ ., data=storeTrain) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) 

  
prepped_rec <- prep(store_recipe)
bake(prepped_rec, storeTrain)

# Random Forests

forest_model <- rand_forest(mtry = tune(),
                            min_n=tune(),
                            trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")


forest_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(forest_model) 

tuning_grid <- grid_regular(mtry(range =c(1,8)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(storeTrain, v = 5, repeats = 2)

forest_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape))

bestTune <- forest_results %>%
  select_best("smape")

# Turn into mean
mean <- collect_metrics(forest_results) %>%
  filter(mtry==bestTune$mtry, min_n==bestTune$min_n, 
         .config==bestTune$.config) %>%
  pull(mean)
mean

library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)
library(lubridate)
library(embed)


storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

#### Exponential smoothing ####

library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)
library(lubridate)
library(embed)
library(modeltime)
library(timetk)

storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

storeTrain36 <- storeTrain %>%
  filter(store==3, item==6)

storeTrain13 <- storeTrain %>%
  filter(store==1, item==3)

storeTest36 <- storeTest %>%
  filter(store==3, item==6)

storeTest13 <- storeTest %>%
  filter(store==1, item==3)

cv_split36 <- time_series_split(storeTrain36, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split36 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split13 <- time_series_split(storeTrain13, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split13 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

store_recipe <- recipe(sales ~ ., data=storeTrain36) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))



es_model36 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split36))

es_model13 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split13))

cv_results36 <- modeltime_calibrate(es_model36, 
                                  new_data=testing(cv_split36))

cv_results13 <- modeltime_calibrate(es_model13, 
                                    new_data=testing(cv_split13))

plot1 <- cv_results36 %>%
  modeltime_forecast(
    new_data = testing(cv_split36), 
    actual_data = storeTrain36
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plot2 <- cv_results13 %>%
  modeltime_forecast(
    new_data = testing(cv_split13), 
    actual_data = storeTrain13
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

cv_results36 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

cv_results13 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

es_fullfit36 <- cv_results36 %>%
  modeltime_refit(data=storeTrain36)

es_fullfit13 <- cv_results13 %>%
  modeltime_refit(data=storeTrain13)
 
es_preds36 <- es_fullfit36 %>%
  modeltime_forecast(h="3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest36, by="date") %>%
  select(id, sales)

es_preds13 <- es_fullfit13 %>%
  modeltime_forecast(h="3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest13, by="date") %>%
  select(id, sales)

plot3 <- es_fullfit36 %>%
  modeltime_forecast(h="3 months", actual_data = storeTrain36) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plot4 <- es_fullfit13 %>%
  modeltime_forecast(h="3 months", actual_data = storeTrain13) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plotly::subplot(plot1, plot2, plot3,plot4, nrows=2)



##### SARIMA #####

library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)
library(lubridate)
library(embed)
library(modeltime)
library(timetk)
library(forecast)

storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

storeTrain36 <- storeTrain %>%
  filter(store==3, item==6)

storeTrain13 <- storeTrain %>%
  filter(store==1, item==3)

storeTest36 <- storeTest %>%
  filter(store==3, item==6)

storeTest13 <- storeTest %>%
  filter(store==1, item==3)

cv_split36 <- time_series_split(storeTrain36, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split36 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split13 <- time_series_split(storeTrain13, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split13 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

store_recipe36 <- recipe(sales ~ ., data=storeTrain36) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

store_recipe13 <- recipe(sales ~ ., data=storeTrain13) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prepped_rec13 <- prep(store_recipe13)
bake(prepped_rec13, storeTrain13)

prepped_rec36 <- prep(store_recipe36)
bake(prepped_rec36, storeTrain36)
next3Months <- storeTest36

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5,
                         non_seasonal_ma =5,
                         seasonal_ar=2,
                         seasonal_ma=2,
                         non_seasonal_differences = 2,
                         seasonal_differences = 2) %>%
  set_engine("auto_arima")

arima_wf36 <- workflow() %>%
  add_recipe(store_recipe36) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split36))

arima_wf13 <- workflow() %>%
  add_recipe(store_recipe13) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split13))

# es_model36 <- exp_smoothing() %>%
#   set_engine("ets") %>%
#   fit(sales~date, data=training(cv_split36))
# 
# es_model13 <- exp_smoothing() %>%
#   set_engine("ets") %>%
#   fit(sales~date, data=training(cv_split13))

cv_results36 <- modeltime_calibrate(arima_wf36, 
                                    new_data=testing(cv_split36))

cv_results13 <- modeltime_calibrate(arima_wf13, 
                                    new_data=testing(cv_split13))


plot1 <- cv_results36 %>%
  modeltime_forecast(
    new_data = testing(cv_split36), 
    actual_data = storeTrain36
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plot2 <- cv_results13 %>%
  modeltime_forecast(
    new_data = testing(cv_split13), 
    actual_data = storeTrain13
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

cv_results36 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

cv_results13 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

es_fullfit36 <- cv_results36 %>%
  modeltime_refit(data=storeTrain36)

es_fullfit13 <- cv_results13 %>%
  modeltime_refit(data=storeTrain13)

es_preds36 <- es_fullfit36 %>%
  modeltime_forecast(new_data=storeTest36,
                     actual_data = storeTrain36) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest36, by="date") %>%
  select(id, sales)

es_preds13 <- es_fullfit13 %>%
  modeltime_forecast(new_data=storeTest13,
                     actual_data = storeTrain13) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest13, by="date") %>%
  select(id, sales)

plot3 <- es_fullfit36 %>%
  modeltime_forecast((new_data=storeTest36), actual_data = storeTrain36) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plot4 <- es_fullfit13 %>%
  modeltime_forecast((new_data=storeTest13), actual_data = storeTrain13) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plotly::subplot(plot1, plot2, plot3,plot4, nrows=2)

##### PROPHET MODELS #####
library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)
library(lubridate)
library(embed)
library(modeltime)
library(timetk)
library(forecast)

storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

storeTrain36 <- storeTrain %>%
  filter(store==3, item==6)

storeTrain13 <- storeTrain %>%
  filter(store==1, item==3)

storeTest36 <- storeTest %>%
  filter(store==3, item==6)

storeTest13 <- storeTest %>%
  filter(store==1, item==3)

cv_split36 <- time_series_split(storeTrain36, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split36 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split13 <- time_series_split(storeTrain13, 
                                assess = "3 months", 
                                cumulative = TRUE)
cv_split13 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

store_recipe36 <- recipe(sales ~ ., data=storeTrain36) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

store_recipe13 <- recipe(sales ~ ., data=storeTrain13) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prepped_rec13 <- prep(store_recipe13)
bake(prepped_rec13, storeTrain13)

prepped_rec36 <- prep(store_recipe36)
bake(prepped_rec36, storeTrain36)
next3Months <- storeTest36

prophet_model13 <- prophet_reg() %>%
  set_engine(engine="prophet") %>%
  fit(sales ~ date, data=training(cv_split13))
prophet_model36 <- prophet_reg() %>%
  set_engine(engine="prophet") %>%
  fit(sales~ date, data=training(cv_split36))

cv_results36 <- modeltime_calibrate(prophet_model36, 
                                    new_data=testing(cv_split36))

cv_results13 <- modeltime_calibrate(prophet_model13, 
                                    new_data=testing(cv_split13))

plot1 <- cv_results36 %>%
  modeltime_forecast(
    new_data = testing(cv_split36), 
    actual_data = storeTrain36
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plot2 <- cv_results13 %>%
  modeltime_forecast(
    new_data = testing(cv_split13), 
    actual_data = storeTrain13
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

cv_results36 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

cv_results13 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=FALSE
  )

es_fullfit36 <- cv_results36 %>%
  modeltime_refit(data=storeTrain36)

es_fullfit13 <- cv_results13 %>%
  modeltime_refit(data=storeTrain13)

es_preds36 <- es_fullfit36 %>%
  modeltime_forecast(h="3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest36, by="date") %>%
  select(id, sales)

es_preds13 <- es_fullfit13 %>%
  modeltime_forecast(h="3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest13, by="date") %>%
  select(id, sales)

plot3 <- es_fullfit36 %>%
  modeltime_forecast(h="3 months", actual_data = storeTrain36) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plot4 <- es_fullfit13 %>%
  modeltime_forecast(h="3 months", actual_data = storeTrain13) %>%
  plot_modeltime_forecast(.interactive=FALSE) 

plotly::subplot(plot1, plot2, plot3,plot4, nrows=2)


