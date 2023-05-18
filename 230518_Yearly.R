# 연간자료

# 데이터 불러오기
library(tidyverse)

getwd()


data_c_2020 <- read.csv("./2020_연간자료_C형.csv" , header = T, fileEncoding = "euc-kr")
data_c_2021 <- read.csv("./2021_연간자료_C형.csv" , header = T, fileEncoding = "euc-kr")
# 파일 안에 한글이 있어 인코딩되지 않을 때  [header = T, fileEncoding = "euc-kr"] 항목 추가

