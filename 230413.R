# 인사관리 데이터를 통한 R 데이터 핸들링 [1]

getwd() # 현재 디렉토리 확인하기

# 데이터 불러오기
DATA = read.csv('C:/Users/NDL/Documents/수업자료/HR_comma_sep.csv')

head(DATA) # 데이터 윗부분 (상위 5개행) 띄우기
str(DATA) # 데이터 strings 파악
summary(DATA) # 요약된 데이터 살펴보기

# numeric : 수치형 (최솟값, 최댓값, 사분위값, 중앙값, 평균 등이 제공)
# factor : 변수가 어떤 인자들로 이루어졌는지 나타내줌 (ex 0, 1 등)

summary(DATA$Work_accident)
summary(DATA$left)
summary(DATA$promotion_last_5years)

DATA$Work_accident = as.factor(DATA$Work_accident)
DATA$left = as.factor(DATA$left)
DATA$promotion_last_5years = as.factor(DATA$promotion_last_5years)

summary(DATA$Work_accident)
summary(DATA$left)
summary(DATA$promotion_last_5years)

## 패키지 설치 되지 않을 때 하단 console창에 options(repos = c(CRAN = 'http:/cran.rstudio.com')) 입력 후 패키지 설치

# 패키지 설치하가
install.packages('ggplot2')

# 패키지 불러오기
library(ggplot2)

# x축 y축 정하기, 시작명령어는 ggplot, 그래프 변수 설정시 무조건 aes 안에 

# barplot (x축만 설정해도 됨)
ggplot(DATA, aes(x = salary)) + geom_bar()

# barplot + 색상변경
ggplot(DATA, aes(x = salary)) + geom_bar(fill = '#579486')
# factor 타입인 변수는 옆에 범례로 띄울 수 있다.
ggplot(DATA, aes(x = salary)) + geom_bar(aes(fill = left))
# 범례, 축 이름 편집
ggplot(DATA, aes(x = salary)) + geom_bar(aes(fill = left)) + labs(fill = 'Divided by left')
ggplot(DATA, aes(x = salary)) + geom_bar(aes(fill = left)) + labs(fill = 'Divided by left') + xlab('봉급 수준') + ylab('')

# 축 순서 변경
DATA$salary = factor(DATA$salary, levels=c('low','medium','high'))
ggplot(DATA, aes(x = salary)) + geom_bar(aes(fill = left)) + labs(fill = 'Divided by left') + xlab('봉급 수준') + ylab('')
ggplot(DATA, aes(x = salary)) + geom_bar(aes(fill = sales)) + labs(fill = 'Divided by left') + xlab('봉급 수준') + ylab('')

# 히스토그램
ggplot(DATA, aes(x= satisfaction_level)) + geom_histogram()
ggplot(DATA, aes(x= satisfaction_level)) + geom_histogram(binwidth=0.01, col = 'red', fill='skyblue')

# 밀도그래프
ggplot(DATA, aes(x= satisfaction_level)) + geom_density()
ggplot(DATA, aes(x= satisfaction_level)) + geom_density(col='yellow', fill='black')

# 박스플롯(box plot)
ggplot(DATA, aes(x = left, y = satisfaction_level)) + geom_boxplot(aes(fill = left)) + xlab('이직여부') + ylab('직무 만족도') + ggtitle('Boxplot') + labs(fill = '이직여부')

ggplot(DATA, aes(x = left, y = satisfaction_level)) + 
  geom_boxplot(aes(fill = left), alpha = I(0.4)) + 
  geom_jitter(aes(col = left), alpha = I(0.4)) + 
  xlab('이직여부') + ylab('만족도') + ggtitle('Boxplot') + 
  labs(fill = '이직 여부', col = '이직 여부')

ggplot(DATA, aes(x = left, y = satisfaction_level)) + 
  geom_boxplot(aes(fill = left), alpha = I(0.4), outlier_colour = 'red') + 
  xlab('이직여부') + ylab('만족도') + ggtitle('Boxplot') + 
  labs(fill = '임금 수준')


# 산점도

head(DATA)
ggplot(DATA, aes(x = average_montly_hours, y = satisfaction_level)) + geom_point()

# 색을 통한 인사이트 발굴. 색상에 축을 넣음 (col=left)
ggplot(DATA, aes(x = average_montly_hours, y = satisfaction_level)) + geom_point(aes(col = left)) + labs(col='이직 여부') + xlab('평균 근무시간') + ylab('만족도')
