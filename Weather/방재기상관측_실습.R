# 작업 디렉토리 경로 설정

getwd()
setwd("./input")
getwd()

# 패키지 로딩

install.packages("readr")

library(data.table)
library(h2o)
library(tidyverse)
library(readr)
library(gridExtra)
library(Metrics)

# 분석 환경 세팅

rm(list=ls())
cat("\014")
Sys.setenv(LANG="en_US.UTF-8")
options(scipen=999)
options(digits=10)

# 데이터 불러오기 : 기상데이터
weather <- read_csv("./weather.csv")
# 불러온 데이터 구조 확인하기
str(weather)

# 도로 기하데이터
traf <- read_csv("./traf.csv")
# 불러온 데이터 구조 확인하기
str(traf)

# 교통류 데이터
road <- read.csv("./road.csv")
str(road)

# 확인결과 교통류 데이터의 데이터를 자료에 나온 결과에 맞게 불러와야 함
getwd()
road <- read_csv("road.csv",
                 col_names = TRUE,
                 col_types = list(.default = col_character())
)

str(road)


road$Y <- as.double(road$Y)
road$BUSlane <- as.double(road$BUSlane)
road$JD_3KM_STD <- as.double(road$JD_3KM_STD)
road$PM_3KM_STD <- as.double(road$PM_3KM_STD)
road$JD_1KM_AVG <- as.double(road$JD_1KM_AVG)
road$KS_3KM_AVG <- as.double(road$KS_3KM_AVG) 
road$BY_3KM_STD <- as.double(road$BY_3KM_STD)
road$BY_3KM_AVG <- as.double(road$BY_3KM_AVG)  
road$SPEEDLIMIT <- as.double(road$SPEEDLIMIT)

str(road)
# 안바뀐다 ㅋ

# 데이터 결합 및 확인

weather_traf <- weather %>%
  left_join(road) %>%
  left_join(traf)

weather_traf


# 타입 변환 

weather_traf$Y <- as.numeric(weather_traf$Y)
weather_traf$AVG_SPEED <- as.numeric(weather_traf$AVG_SPEED)
weather_traf$VOLUME_ALL <- as.numeric(weather_traf$VOLUME_ALL)
weather_traf$TA_MAX_2_STD <- as.numeric(weather_traf$TA_MAX_2_STD)
weather_traf$WS_MAX_6_STD <- as.numeric(weather_traf$WS_MAX_6_STD)
weather_traf$WS_MAX_2_MIN <- as.numeric(weather_traf$WS_MAX_2_MIN)
weather_traf$TA_MIN_6_MAX <- as.numeric(weather_traf$TA_MIN_6_MAX)
weather_traf$WS_AVG_2_STD <- as.numeric(weather_traf$WS_AVG_2_STD)
weather_traf$TA_MIN_6_STD <- as.numeric(weather_traf$TA_MIN_6_STD)
weather_traf$RN_3_MAX <- as.numeric(weather_traf$RN_3_MAX)
weather_traf$month <- as.character(weather_traf$month)
weather_traf$hour <- as.character(weather_traf$hour)
weather_traf$GISID <- as.character(weather_traf$GISID)
weather_traf$CONZONE <- as.character(weather_traf$CONZONE)
weather_traf$BUSlane <- as.character(weather_traf$BUSlane)
weather_traf$JD_3KM_STD <- as.numeric(weather_traf$JD_3KM_STD)
weather_traf$PM_3KM_STD <- as.numeric(weather_traf$PM_3KM_STD)
weather_traf$JD_1KM_AVG <- as.numeric(weather_traf$JD_1KM_AVG)
weather_traf$KS_3KM_AVG <- as.numeric(weather_traf$KS_3KM_AVG)
weather_traf$BY_3KM_STD <- as.numeric(weather_traf$BY_3KM_STD)
weather_traf$BY_3KM_AVG <- as.numeric(weather_traf$BY_3KM_AVG)
weather_traf$SPEEDLIMIT <- as.numeric(weather_traf$SPEEDLIMIT)


# 데이터 요약, 결측치 파악

# 데이터 요약
summary(weather_traf)

# 컬럼별 결측치
map_dfr(1:22, function(x) {
  data.frame(
    col = colnames(weather_traf[x]),
    count_na = sum(is.na(weather_traf[x]))
  )
})

# 1km 구간에 따른 사고 빈도 (기상)

lst <- map(5:11, function(i) {
  mean_value <- colnames(weather_traf[i])
  
  weather_traf %>% 
    select(GISID, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(GISID) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#208A2E") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)


# 1KM 구간에 따른 사고 빈도 (기하구조)

lst <- map(14:20, function(i) {
  mean_value <- colnames(weather_traf)[i]
  
  weather_traf %>% 
    select(GISID, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(GISID) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#A28C2A") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)

# 1KM 구간에 따른 사고 빈도 (교통류)

lst <- map(21:22, function(i) {
  mean_value <- colnames(weather_traf)[i]
  
  weather_traf %>% 
    select(GISID, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(GISID) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#2125A1") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)

# ___콘존에 따른 사고 빈도 ----

# 콘존을 칭하는 CONZONE에 대해 각 Y는 SUM(), 각각의 하위 데이터는 Mean()을 하여 시각화
# 색상코드
# 기상: #208A2E
# 기하: #A28C2A
# 교통: #2125A1

# ____ 기상 ----

# 콘존 따른 사고 빈도 (기상)

lst <- map(5:11, function(i) {
  mean_value <- colnames(weather_traf)[i]
  
  weather_traf %>% 
    select(CONZONE, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(CONZONE) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#208A2E") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)


# ____ 기하구조 ----

lst <- map(14:20, function(i) {
  mean_value <- colnames(weather_traf)[i]
  
  weather_traf %>% 
    select(CONZONE, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(CONZONE) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#A28C2A") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)

# ____ 교통류----

lst <- map(21:22, function(i) {
  mean_value <- colnames(weather_traf)[i]
  
  weather_traf %>% 
    select(CONZONE, mean_value, Y) %>% 
    rename(aa = mean_value) %>%
    group_by(CONZONE) %>% 
    summarise(sum_y = sum(Y), 
              mean_value = mean(aa)) %>% 
    select(sum_y, mean_value) %>%
    rename(aa = mean_value) %>%
    ggplot(aes(x = aa,y = sum_y)) +
    geom_point(alpha=.7,color = "#2125A1") +
    labs(title = paste0(mean_value," by count"), x = mean_value, y = "count") +
    theme_bw() +
    theme(legend.position = "bottom")
})

grid.arrange(grobs=lst, ncol=2)



# 히스토그램 (발생건수, 빈도)

hist(weather_traf$Y,
     xlab = "Count",
     ylab = "Frequency",
     main = "Frequency by count")



# 3. 데이터 처리 ----
# _이상치 제거 ----
#=============================================================
# 이상치 제거 전

summary(weather_traf)

# 이상치 제거

weather_traf <- weather_traf %>% 
  filter(TA_MAX_2_STD < 7) %>% # 이전 2시간 동안 최대 기온의 편차
  filter(WS_MAX_2_MIN < 7) %>% # 이전 2시간 동안 최대 풍속의 최소
  filter(TA_MIN_6_STD < 10) %>% # 이전 6시간 동안 최소 기온의 편차
  filter(RN_3_MAX < 20) %>% # 이전 3시간 동안 강수의 최대
  filter(PM_3KM_STD < 10000)  # 이전 3KM 구간 동안 평면 편차


# 이상치 제거 후

summary(weather_traf)



# 4. 모형 구축 ----
# __ 분석 데이터 셋 ----

# 로컬에 H2O 가상서버 설정하기

h2o.init(nthreads = -1) # h2o 서버 세팅
h2o.removeAll()


# 데이터 분할

# set x and y
df_h2o_1 <- as.h2o(weather_traf)
x        <- which(colnames(df_h2o_1)!="Y")
y        <- which(colnames(df_h2o_1)=="Y")
colnames(df_h2o_1)[x]
colnames(df_h2o_1)[y]

# split dataset
split <- h2o.splitFrame(df_h2o_1, ratios = c(0.6, 0.2), seed=1234)
train <- split[[1]]
valid <- split[[2]]
test <- split[[3]]

#=============================================================
h2o_train <- as.h2o(train)
h2o_valid <- as.h2o(valid)
h2o_test <- as.h2o(test)

# __모형 구축 ----
#============================================================
# 모형 튜닝 자동화
#============================================================
# catesian grid search
grid_params <- list(learn_rate = c(0.1, 0.05, 0.001),
                    max_depth = c(12,15,18))

# Train and validate a random grid of GBMs
gbm_grid <- h2o.grid("gbm", 
                     x = x,
                     y = y,
                     grid_id = "gbm_grid",
                     training_frame = train,
                     validation_frame = valid,
                     hyper_params = grid_params,
                     col_sample_rate = 0.85,
                     col_sample_rate_change_per_level = 0.9,
                     col_sample_rate_per_tree = 0.6,
                     histogram_type = "UniformAdaptive",
                     min_rows = 16384.0,
                     min_split_improvement = 0.0,
                     nbins = 1024,
                     nbins_cats = 2048,
                     ntrees = 148,
                     seed = 1234
)

# RMSE가 낮은순으로 정렬하기
gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "rmse",
                            decreasing = FALSE)
gbm_gridperf

#============================================================
# 모형 선택
#============================================================
# create Model 
Traffic_GBM_Model <- h2o.gbm(training_frame = train
                             ,validation_frame = valid
                             ,col_sample_rate = 0.85
                             ,col_sample_rate_change_per_level = 0.9
                             ,col_sample_rate_per_tree = 0.6
                             ,histogram_type = "UniformAdaptive"
                             ,max_depth = 12
                             ,learn_rate = 0.1
                             ,min_rows = 16384.0
                             ,min_split_improvement = 0.0
                             ,nbins = 1024
                             ,nbins_cats = 2048
                             ,sample_rate = 0.8
                             ,model_id = "Traffic_GBM_Model"
                             ,ntrees = 148
                             ,x = x
                             ,y = y
                             ,seed = 1234
)
# __Model Summary ----
Traffic_GBM_Model



# 5. 모형 검증 ----
#=============================================================
# __변수 중요도 파악 ----
#=============================================================
feature_list <- h2o.varimp(Traffic_GBM_Model)
feature_list
h2o.varimp_plot(Traffic_GBM_Model)

#============================================================
# __최종 모형 선택 ----
#============================================================
feature_test <- map_dfr(c(3,5,7,9), function(z) {
  train_feature <- train[,c("CONZONE","GISID", "month", "hour", "Y",
                            feature_list$variable[1:z])]
  valid_feature <- valid[,c("CONZONE","GISID", "month", "hour", "Y",
                            feature_list$variable[1:z])]
  
  x        <- which(colnames(train_feature)!="Y")
  y        <- which(colnames(valid_feature)=="Y")
  
  # create Model 
  Traffic_GBM_Model_feature <- h2o.gbm(training_frame = train_feature
                                       ,validation_frame = valid_feature
                                       ,col_sample_rate = 0.85
                                       ,col_sample_rate_change_per_level = 0.9
                                       ,col_sample_rate_per_tree = 0.6
                                       ,histogram_type = "UniformAdaptive"
                                       ,max_depth = 12
                                       ,learn_rate = 0.1
                                       ,min_rows = 16384.0
                                       ,min_split_improvement = 0.0
                                       ,nbins = 1024
                                       ,nbins_cats = 2048
                                       ,sample_rate = 0.8
                                       ,model_id = "Traffic_GBM_Model"
                                       ,ntrees = 148
                                       ,x = x
                                       ,y = y
                                       ,seed = 1234
  )
  
  # Model rmse
  data.frame(
    feature = z,
    rmse = Traffic_GBM_Model_feature@model$validation_metrics@metrics$RMSE
  )
})

feature_test

# _모형 성능 및 예측력 파악 ----
#=============================================================
# 모형 성능
#=============================================================
train_feature <- train[,c("CONZONE","GISID", "month", "hour", "Y",
                          feature_list$variable[1:9])]
test_feature <- valid[,c("CONZONE","GISID", "month", "hour", "Y",
                         feature_list$variable[1:9])]

x        <- which(colnames(train_feature)!="Y")
y        <- which(colnames(test_feature)=="Y")

# create Model 
Traffic_GBM_Model_final <- h2o.gbm(training_frame = train_feature
                                   ,validation_frame = test_feature
                                   ,col_sample_rate = 0.85
                                   ,col_sample_rate_change_per_level = 0.9
                                   ,col_sample_rate_per_tree = 0.6
                                   ,histogram_type = "UniformAdaptive"
                                   ,max_depth = 12
                                   ,learn_rate = 0.1
                                   ,min_rows = 16384.0
                                   ,min_split_improvement = 0.0
                                   ,nbins = 1024
                                   ,nbins_cats = 2048
                                   ,model_id = "Traffic_GBM_Model"
                                   ,ntrees = 148
                                   ,x = x
                                   ,y = y
                                   ,seed = 1234
)
# __Model Summary ----
Traffic_GBM_Model_final

#=============================================================
# 모형 성능
#=============================================================
# Predict on test set
pred_conversion <- h2o.predict(object = Traffic_GBM_Model_final, newdata = test_feature)

pred_conversion <- pred_conversion %>% 
  as_tibble()

comp <- cbind(as_tibble(test_feature$Y),pred_conversion)

# __RMSE ----
rmse(comp$Y, comp$predict)


#  RMSE

comp %>% 
  ggplot(aes(x = Y, y = predict)) +
  geom_point() + 
  labs(title= paste0("rmse=", rmse(comp$Y, comp$predict)), x = "TRUE", y = "Prediction") + 
  theme_bw()

