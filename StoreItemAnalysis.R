library(tidyverse)
library(tidymodels)
library(vroom)
library(gridExtra)


storeTrain <- vroom("./train.csv")
storeTest <- vroom("./test.csv")

# We want to create a ACF plot that shows the differences
# in the autocorrelation structure for the different items & upload the image to LS

# take out rows where item and store ==1
acf11 <- storeTrain %>%
  filter(store==1, item==1)
acf44 <- storeTrain %>%
  filter(store==4,item==4)
acf22<- storeTrain %>%
  filter(store==2,item==2)
acf33 <- storeTrain %>%
  filter(store==3,item==3)

acf11 %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf22 %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf33 %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf44 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

par(mfrow=c(2,2))

plot1 <- acf11 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)  + ggtitle("Store=1,Item=1,Lag=2years")


plot2 <- acf22 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=2,Item=2,Lag=2years")


plot3 <- acf33 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=3,Item=3,Lag=2years")


plot4 <- acf44 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) + ggtitle("Store=4,Item=4,Lag=2years")


grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Save the arranged plot using ggsave
ggsave("output_plot1.png", width = 10, height = 8)


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