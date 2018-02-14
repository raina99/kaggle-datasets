library(geosphere)
library(RColorBrewer)
library(ggmap)
library(data.table)
library(ggplot2)
library(dplyr) ## dustinct function in dplyr
library(lubridate)
library(fasttime)
library(readr)
library(magrittr)
dir()
### Ideas without seeing the dataset
#### expensive cars need insurances so they don't lose value # price
### safe driver don't prefer insurance # good driving skills 
# teenagers are more  likely to get insurance# age
#  middle class people less  as compared to rich #income 
# areas with most accidents #states 
# regular trips # usage 
# distance of trips  # distance  
# 
# Goal - Company  wants to predict which of good drivers and which bad drivers will file
#  insurance and that depends on the probability of accident.
#  A good driver without doing anything silly  can also be involved in a car damage car due to 
#  A bad driver or other external factors too
##  NORMALIZED GINI  COEFFICENT 

train = read.csv("train.csv",na.strings = c("",NA))
test = read.csv("test.csv",na.strings = c("",NA))
sample_sub = read.csv("sample_submission.csv")

### Data exploration
glimpse(train)
summary(train)
glimpse(test)
summary(test)
## Train
## missing values present in some of the columns
# target variable   has Mean - 0.03645  and Median - 0.0  and Max value - 1
# target variable  0 - not filed claim  and 1 - filed claim
## Test
## missing values present in some of the columns

##Missing value
# replacing all -1 values with NA
train[train == -1] <- NA 
test[test == -1] <- NA
## Checking whether all -1 values have been removed 
which(train == -1)
which (test == -1)
#
sum(is.na(train)) # a large no of NA present
colSums(is.na(train))
#ps_car_05_cat ,ps_car_03_cat, ps_reg_03,ps_car_01_cat,ps_car_07_cat, ps_car_14,ps_ind_02_cat,  
#ps_ind_04_cat, ps_ind_05_cat # 9 columns have missing values

colSums(is.na(train)/nrow(train))
colSums(is.na(train))
(colSums((is.na(train)/nrow(train))) *100)
## Data visualization 
## observing count of different categories of variables 
## so that later we can get some insight from distribution

names (train)
# ind variables 
unique(train$ps_ind_01)

ggplot(data = train,mapping = aes(x =ps_ind_01,fill = as.factor(ps_ind_01) ))+
geom_bar()
## value 0 showing the peak followed by others 1,2,3,4,5,6,7

ggplot(data = train,mapping = aes(x =ps_ind_03,fill = as.factor(ps_ind_03) ))+
  geom_bar()


ggplot(data = train,mapping = aes(x =ps_ind_14,fill = as.factor(ps_ind_14) ))+
  geom_bar()
## 0 is the major value  and other are very less

ggplot(data = train,mapping = aes(x =ps_ind_14,fill = as.factor(ps_ind_14) ))+
  geom_bar()

## catergorical variables represented by cat
ggplot(data = train,mapping = aes(x =ps_ind_02_cat,fill = as.factor(ps_ind_02_cat ) ))+
  geom_bar()

ggplot(data = train,mapping = aes(x =ps_ind_04_cat,fill = as.factor(ps_ind_04_cat ) ))+
  geom_bar()


ggplot(data = train,mapping = aes(x =ps_ind_05_cat,fill = as.factor(ps_ind_05_cat ) ))+
  geom_bar()
## car_cat variables
ggplot(data = train,mapping = aes(x = ps_car_01_cat,fill = as.factor(ps_car_01_cat ) ))+
  geom_bar()

ggplot(data = train,mapping = aes(x = ps_car_02_cat,fill = as.factor(ps_car_02_cat ) ))+
  geom_bar()

ggplot(data = train,mapping = aes(x = ps_car_03_cat,fill = as.factor(ps_car_03_cat ) ))+
  geom_bar()


ggplot(data = train,mapping = aes(x = ps_car_04_cat,fill = as.factor(ps_car_04_cat ) ))+
  geom_bar()


ggplot(data = train,mapping = aes(x = ps_car_05_cat,fill = as.factor(ps_car_05_cat ) ))+
  geom_bar()

ggplot(data = train,mapping = aes(x = ps_car_06_cat,fill = as.factor(ps_car_06_cat ) ))+
  geom_bar()


ggplot(data = train,mapping = aes(x = ps_car_07_cat,fill = as.factor(ps_car_07_cat ) ))+
  geom_bar()

ggplot(data = train,mapping = aes(x = ps_car_08_cat,fill = as.factor(ps_car_08_cat ) ))+
  geom_bar()+

##  bin variables

ggplot(data =train,mapping = aes(x = ps_ind_06_bin,fill = as.factor(ps_ind_06_bin ) ))+
geom_bar()
  
ggplot(data =train,mapping = aes(x = ps_ind_07_bin,fill = as.factor(ps_ind_07_bin ) ))+
  geom_bar()


ggplot(data =train,mapping = aes(x = ps_ind_08_bin,fill = as.factor(ps_ind_08_bin ) ))+
  geom_bar()


ggplot(data =train,mapping = aes(x = ps_ind_09_bin,fill = as.factor(ps_ind_09_bin ) ))+
  geom_bar()

## calc variables

ggplot(data =train,mapping = aes(x =  as.factor(ps_calc_01),fill = as.factor(ps_calc_01 ) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x =  as.factor(ps_calc_02),fill = as.factor(ps_calc_02) ))+
  geom_bar()
 
## similarly we see all calc variables distributions


## target variable distribution
ggplot(data =train,mapping = aes(x =  as.factor(target),fill = as.factor(target) ))+
  geom_bar()
## majority of values are non -claim  and few are claims

## Observing trends of diff variables with target 

ggplot(data =train,mapping = aes(x = ps_ind_01 ,fill= as.factor(target) ))+
geom_bar()

## non claims as we know has least values for all unique values of ind_01
## same is the case for all ps_ind variables

## cat variables and target 

ggplot(data =train,mapping = aes(x = ps_ind_02_cat ,fill= as.factor(target) ))+
  geom_bar()

# same pattern follows for cat_variables too



## ind_bin

ggplot(data =train,mapping = aes(x = ps_ind_06_bin,fill= as.factor(target) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x = ps_ind_08_bin,fill= as.factor(target) ))+
  geom_bar()



###ps_calc_15_bin


ggplot(data =train,mapping = aes(x =ps_calc_15_bin ,fill= as.factor(target) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x =ps_calc_16_bin ,fill= as.factor(target) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x =ps_calc_17_bin ,fill= as.factor(target) ))+
  geom_bar()

## ps_calc_01
ggplot(data =train,mapping = aes(x =ps_calc_01,fill= as.factor(target) ))+
  geom_bar()


ggplot(data =train,mapping = aes(x =ps_calc_02,fill= as.factor(target) ))+
  geom_bar()

### majority of different values within variable have same count.

## reg variables

ggplot(data =train,mapping = aes(x =ps_reg_01,fill= as.factor(target) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x =ps_reg_02,fill= as.factor(target) ))+
  geom_bar()

ggplot(data =train,mapping = aes(x =ps_reg_02,fill= as.factor(target) ))+
  geom_bar()


#### 
# Missing value analyis
colSums(is.na(train))


# ps_reg_03 - 18 percent
# ps_car_03_cat -   69 percent
# ps_car_05_cat - 44 percent
# we will discard these columns with so high missing values 
# rest of the column having missing values have values quite low so we will impute them
# train
train$ps_reg_03 = NULL
train$ps_car_03_cat = NULL
train$ps_car_05_cat = NULL

# test
test$ps_reg_03 = NULL
test$ps_car_03_cat = NULL
test$ps_car_05_cat = NULL

colSums(is.na(test))



# 

colSums(is.na(train))
## checking which method of imputation will suit the best
unique(train$ps_ind_02_cat )
train$ps_ind_02_cat[2] = NA # actual value is 1

## checking mean 
train$ps_ind_02_cat[is.na(train2$ps_ind_02_cat)] = mean(train2$ps_ind_02_cat,na.rm = TRUE)
train$ps_ind_02_cat[2] 
train$ps_ind_02_cat[2]  = NA
## mean gives us 1.3598
##
train$ps_ind_02_cat[is.na(train$ps_ind_02_cat)] = median(train$ps_ind_02_cat,na.rm = TRUE)
## median gives us exact value # 1
# other methods such as knn and MICE imputation method are computationally taking too much time

## IMputing values using median
train$ps_ind_04_cat[is.na(train$ps_ind_04_cat)] = median(train$ps_ind_04_cat,na.rm = TRUE)

train$ps_ind_05_cat[is.na(train$ps_ind_05_cat)] = median(train$ps_ind_05_cat,na.rm = TRUE)

train$ ps_car_01_cat  [is.na(train$ ps_car_01_cat  )] = median(train$ ps_car_01_cat,na.rm = TRUE)

train$ ps_car_02_cat  [is.na(train$ ps_car_02_cat  )] = median(train$ ps_car_02_cat,na.rm = TRUE)

train$ ps_car_07_cat  [is.na(train$ ps_car_07_cat  )] = median(train$ ps_car_07_cat,na.rm = TRUE)

train$ ps_car_09_cat  [is.na(train$ ps_car_09_cat  )] = median(train$ ps_car_09_cat,na.rm = TRUE)

train$  ps_car_11    [is.na(train$  ps_car_11    )] = median(train$  ps_car_11,na.rm = TRUE)

train$  ps_car_12    [is.na(train$  ps_car_12    )] = median(train$  ps_car_12,na.rm = TRUE)

train$  ps_car_14    [is.na(train$  ps_car_14    )] = median(train$  ps_car_14,na.rm = TRUE)

## checking whether all NA have been imputed or not
colSums(is.na(train))
sum(is.na(train))

### Imputing values for test
test$  ps_ind_02_cat  [is.na(test$ ps_ind_02_cat )] = median(test$ ps_ind_02_cat,na.rm = TRUE)

test$  ps_ind_04_cat  [is.na(test$ ps_ind_04_cat )] = median(test$ ps_ind_04_cat,na.rm = TRUE)

test$  ps_ind_05_cat  [is.na(test$ ps_ind_05_cat )] = median(test$ ps_ind_05_cat,na.rm = TRUE)

test$  ps_car_01_cat  [is.na(test$  ps_car_01_cat)] = median(test$  ps_car_01_cat,na.rm = TRUE)

test$  ps_car_07_cat  [is.na(test$  ps_car_07_cat)] = median(test$  ps_car_07_cat,na.rm = TRUE)
test$  ps_car_02_cat  [is.na(test$  ps_car_02_cat)] = median(test$  ps_car_02_cat,na.rm = TRUE)

test$  ps_car_09_cat  [is.na(test$  ps_car_09_cat)] = median(test$  ps_car_09_cat,na.rm = TRUE)

test$ ps_car_11  [is.na(test$ ps_car_11 )] = median(test$ ps_car_11,na.rm = TRUE)

test$ ps_car_14  [is.na(test$ ps_car_14 )] = median(test$ ps_car_14,na.rm = TRUE)


## 
sum(is.na(test))
colSums(is.na(test))
### outliers

boxplot(train$ps_ind_02_cat)
boxplot(train$ps_calc_13)
table(train$ps_reg_02)
boxplot(train$ps_reg_02)
## The values shown the boxplot at the extreme end are present in good proprtion in dataset
#  Thus not they are not exactly outliers  as they show diffrent factors

###### Model preparation
## creating  target variable  for test

test$target = 0

## converting all columnns to numeric 

train[] <- lapply(train, function(x) as.numeric(as.character(x)))

## checking whether any NA induced 

sum (is.na(train)
## We will use H2o as dataset is large and it will take large computational time

## installing h2o
library (h2o) 
 h2o.init ()

## converting data into h2o 
train.h2o = as.h2o (train)
test.h2o = as.h2o (test)
train$target = as.factor(train$target)
 colnames (train.h2o)
 y.dep = 2
 x.indep = c(3:56)
 library (glm2)
  
 ## Runiing algorithms to see our base accuracy 
 ## using cross validation to check gini score on validation dataset
 
 library(caret)
 library(lattice)
 traincv <- createDataPartition(train$target, p = 0.8, list = FALSE, times = 1)
 
 train1 <- train[traincv,]
 valid1 <- train[-traincv,]
 
 train1.h2o = as.h2o(train1)
 valid1.h2o = as.h2o(valid1)
 
 glm.model2 <- h2o.glm( y = y.dep, x = x.indep, training_frame = train1.h2o, validation_frame = valid1.h2o
,family = "binomial")
  
h2o.performance(glm.model)




library(MLmetrics)
library(Matrix)
library(caret)
install.packages("MLmetrics")



predict_glm2 = as.data.frame(h2o.predict(glm.model,test.h2o,type = "response"))
predict_glm2
solution_glm3= data.frame(id = test$id,target = predict_glm2$p1)

# checking dim
identical(dim(sample_sub),dim(solution_glm2))
## writing submission file 
write.csv(solution_glm1,file = "glm1.csv",row.names = FALSE)

## gives a score of 0.13781 on public leaderbpard (1 rank)
## gives a score of 0.1422 on private leaderbaord (1 rank)
##  top accuracy on private leaderboard is 0.29698

### evaluation metrics

## using p1 in predictions of glm

predict_glm3 = as.data.frame(h2o.predict(glm.model,test.h2o,type = "response")) 
solution_glm3= data.frame(id = test$id,target = predict_glm2$p1) ## using one of two predicted probabilites
## as final probabilities

write.csv(solution_glm3,file = "glm3.csv",row.names = FALSE)
##    public leader board score 0.23683 (rank 1)
## private leaderboard score  0.24194 ( rank 1)
## top accuracy on private leaderboard is 0.29698  (present rank 1)

#### using deep learning 
library(h2o)
h2o.init()

dlearning.model = h2o.deeplearning(y = y.dep, x = x.indep,training_frame = train1.h2o
,validation_frame = valid1.h2o,epoch = 60,seed = 1122,hidden = c(100,100),activation = "Rectifier")

predict_dl = as.data.frame(h2o.predict(dlearning.model,test.h2o,type = "response")) 

solution_dl= data.frame(id = test$id,target = predict_dl$predict) ## using one of two predicted probabilites
## as final probabilities

write.csv(solution_dl,file = "dl.csv",row.names = FALSE)














model_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score <- NormalizedGini(preds,actual)
  return(list(metric = "NormalizedGini", value = score))
}

NormalizedGini(predict_glm2$p0,valid1$target)
valid1$target = as.numeric(valid1$target)

#### using proc




