# 인사관리 데이터를 통한 R 데이터 핸들링 [1]

# 데이터 불러오기
DATA = read.csv('C:/Users/NDL/Documents/수업자료/HR_comma_sep.csv')

head(DATA) # 데이터 윗부분 띄우기
str(DATA) # 데이터 strings 파악
summary(DATA) # 요약된 데이터 살펴보기
