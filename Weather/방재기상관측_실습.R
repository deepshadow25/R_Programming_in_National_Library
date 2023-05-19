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

# 확인결과 교통류 데이터의 데이터를 자료에 나온 결과에 맞게 형변환 해줘야 한다.

getwd()
road <- read_csv(readr_example("road.csv"),
                 col_types = list(.default = col_character())
)

str(road)

road$month <- as.character(road$month)
road$hour <- as.character(road$hour)
road$Y <- as.double(road$Y)
road$GISID <- as.character(road$GISID)
road$CONZONE <- as.character(road$CONZONE)
road$BUSlane <- as.double(road$BUSlane)
road$JD_3KM_STD <- as.double(road$JD_3KM_STD)
road$PM_3KM_STD <- as.double(road$PM_3KM_STD)
road$JD_1KM_AVG <- as.double(road$JD_1KM_AVG)
road$KS_3KM_AVG <- as.double(road$KS_3KM_AVG) 
road$BY_3KM_STD <- as.double(road$BY_3KM_STD)
road$BY_3KM_AVG <- as.double(road$BY_3KM_AVG)  
road$SPEEDLIMIT <- as.double(road$SPEEDLIMIT)

str(road)

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
