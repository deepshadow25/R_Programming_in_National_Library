# 실습문제

# Q1 대소비교 함수

myfunc = function(a, b)
{
  if (a > b){print(paste(a,'>',b))}
  else if (a < b){print(paste(a,'<',b))}
  else{print(paste(a,'=',b))}
}

myfunc(31,31)
myfunc(1,3)
myfunc(112,47)


# Q2  구구단

myfunc = function(n)
{
  for (i in c(1:n)){
    print(n*i)
  }
}
myfunc(9)


# Q3  데이터 비교

myfunc = function(data1, data2)
{
  print('mean', mean(data1))
}
