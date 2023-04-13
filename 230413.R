# 인사관리 데이터를 통한 R 데이터 핸들링 [1]

getwd() # 현재 디렉토리 확인하기

# 데이터 불러오기
DATA = read.csv('C:/Users/NDL/Documents/수업자료/HR_comma_sep.csv')

head(DATA) # 데이터 윗부분 (상위 5개행) 띄우기
str(DATA) # 데이터 strings 파악
summary(DATA) # 요약된 데이터 살펴보기

# 데이터 유형이 numeric일 때와 factor일 때 나타나는 값이 다르다.
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
