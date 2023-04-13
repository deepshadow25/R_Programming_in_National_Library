# 234012 R code

# 할당(=, <-), 논리문(==)
# ctrl + enter
a <-2
a

a == 2 # a가 2인지 판단
a != 2 # a가 2인지 판단

# 변수명은 가급적 영어로 작성

# 괄호 - 소괄호:(), 중괄호:{}, 대괄호:[]
# 소괄호 : () 실행함수(function)와 함께 쓰임
a=c(1,2,3,4,5)
a

# 중괄호 : [] for,if문에서 조건을 입력할 때
for (i in a) {
  print(a)
}

# 대괄호 : [] 인덱스를 입력해야 할 때 사용
b = c(3,5,7,9)
b[1] # b라는 변수에서 첫번째 값을 인덱싱
b[3] # b라는 변수에서 세번째 값을 인덱싱
b[1:3] # b라는 변수에서 첫-세번째 값을 인덱싱

c = c("종우","데이터","분석","강사")

# as와 is
# as : 변수 x를 ~로 취급하겠다 
# is : 논리문으로써 변수 x가 ~인지 판단하여라
x = c(1,2,3,4,5,6,7,8,9,10)

x1 = as.integer(x)
x2 = as.numeric(x)
x3 = as.factor(x)
x4 = as.character(x)

str(x1)
summary(x1)

str(x2)
summary(x2)

str(x3)
summary(x3)

str(x4)
summary(x4)

# is 데이터 타입 확인
x = c(1,2,3,4,5,6,7,8,9,10)
y = c('1','2','3','4','5','6','7','8','9','10') # 문자열로
# 따옴표를 넣어서 작성

is.integer(x)
is.numeric(x)

is.factor(y)
is.character(y)

# rep() : repeat, seq() : sequence
x1 = seq(1,10,1) # 1~10 1씩 증가하는 수열을 생성
x2 = seq(1,10,3) # 1~10 3씩 증가하는 수열을 생성
x3 = rep(1,10) # 1을 10개 만들겠다
x3

# {} 자동문, 조건문
# 자동문(for문)

for (i in 1:5) { # i(매개변수)에 1부터 5까지 정수를 차례대로 부여
  print(i) # i를 출력
}

list = seq(1,30,3) # 1~30까지 3씩 증가하는 정수로 구성
list
space = c() # 
for (i in list) { # i에 리스트 값들을 순차 접근
  space = c(space, i) # 빈공간에 i값들을 누적 할당
}
space

a = c(1,2,3,4,5,7)
a

if (7 %in% a){ # %in%이란, 백터 내 특정 값 포함 여부 확인
  print("a라는 변수 안에 7이 존재합니다.")
} else {
  print("7이 존재하지 않습니다.")
}

