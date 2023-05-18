# 데이터 불러오기
library(tidyverse)

getwd()

guess_encoding("./2020_연간자료_C형.csv")

data_c_2020 <- read.csv("./2020_연간자료_C형.csv" , header = T, fileEncoding = "euc-kr")
data_c_2021 <- read.csv("./2021_연간자료_C형.csv" , header = T, fileEncoding = "euc-kr")
# 파일 인코딩이 euc-kr일 경우[header = T, fileEncoding = "euc-kr"] 항목 추가
# locale = locale(encoding = "euc-kr") 을 추가해도 됨

dim(data_c_2020)
dim(data_c_2021)

glimpse(data_c_2021)

# data_c_2021 컬럼을 생성해서 2021 값 부여
data_c_2021 <- data_c_2021 %>% mutate(년도 = 2021)
# data_c_2020 컬럼을 생성해서 2021 값 부여
data_c_2020 <- data_c_2020 %>% mutate(년도 = 2020)

# 2020년도, 2021년도 데이터 합치기 (row 기준으로 합쳐주기)
bind_rows(data_c_2020, data_c_2021) -> edu_df

# 데이터 확인하기

edu_df$년도 %>% unique()
edu_df$지역구분코드 %>% unique()
edu_df$학교급구분코드 %>% unique()

## 파일설계.xlsx 파일 참조하여 학교급, 지역 구분코드별 의미 확인



# 우리의 목표
# 1. 통계청에서 나온 수치 계산과정 이해
# 1.1 표본조사에 대한 가중치 이해
# 1.2 사교육비 총액 구해보기
# 1.3 초중고 학교구분별 사교육비 총합
# 1.4 사교육비 증감 비교 : 2020, 2021
# 1.5 막대그래프를 통해 확인

## 주어진 데이터는 설문조사 결과이므로 모집단 가정 - 표본이 모집단을 대표할 수 있음 - 을 해야 함.
## 표본에 따른 가중치 - 표본이 뽑힌 확률의 역수 (표본이 모집단에서 차지하는 정도)

edu_df$가중값

info_df <- edu_df %>%
  mutate(학교급구분코드 = 
           case_when(
             학교급구분코드 == 1 ~ "초등학교", 학교급구분코드 == 2 ~ "중학교",
             TRUE ~ "고등학교"
           )) %>% # TRUE 전부 대문자
  mutate(조정값 = 사교육비총비용 * 가중값) %>%
  group_by(학교급구분코드, 년도) %>%
  summarize(총비용 = sum(조정값) / 10^8) %>% # 조 단위
  ungroup()

# pivot_wider : 행, 열을 재조정하여 데이터를 더 보기 좋게 만드려 할 때 사용하는 함수

info_df2 <- info_df %>%
  pivot_wider(
    id_cols = 학교급구분코드, 
    names_from = 년도,
    values_from = 총비용,
    names_glue = "{년도}년"
  )
