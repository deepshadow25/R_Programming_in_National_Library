# 앞으로 다루어 볼 것 - 통계청 데이터 활용

# - KOSIS 데이터 : R언어 tdiyverse
# - MDIS 데이터 : tdiyverse, GIS 데이터

# 자주 쓰이는 분석 환경 설정 함수

getwd()        # 현재 작업하는 디렉토리 확인
setwd('d:/R')  # 작업할 디렉토리 위치 변경

## 패키지 설치 되지 않을 때 하단 console창에 options(repos = c(CRAN = 'http:/cran.rstudio.com')) 입력 후 패키지 설치
# 대소문자, 띄어쓰기 유의

install.package(ggplot2)

getwd()
data <- read.csv("./HR_comma_sep (1).csv")
data

summary(data)

# describe 함수 사용
install.packages("psych")
library(psych)

describe(data)
describeBy(data, group = 'salary')

# 박스플랏 (y : 연속형, x : 범주형)
ggplot(data, aes(x= salary, y = satisfaction_level)) + geom_boxplot()

str(data)
data$salary <- factor(data$salary, levels = c("low", "medium", "high"))
ggplot(data, aes(x = salary, y = satisfaction_level)) + geom_boxplot()

data$left <- as.factor(data$left)
ggplot(data, aes(x = salary, y = satisfaction_level)) + geom_boxplot(
  aes(fill = left))

data$left <- as.factor(data$left)
ggplot(data, aes(x = salary, y = satisfaction_level)) + 
  geom_boxplot(  aes(fill = left)) +
  xlab('연봉수준') + ylab('만족도') + labs(fill = '이직여부') +
  ggtitle('BOX PLOT')
