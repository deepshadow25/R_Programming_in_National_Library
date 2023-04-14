# 실습문제

# Q1 대소비교 함수

myfunc1 = function(a, b)
{
  if (a > b){print(paste(a,'>',b))}
  else if (a < b){print(paste(a,'<',b))}
  else{print(paste(a,'=',b))}
}

myfunc1(31,31)
myfunc1(1,3)
myfunc1(112,47)


# Q2  구구단

myfunc2 = function(n)
{
  for (i in c(1:n)){
    print(paste(n,'x',i,'=',n*i))
  }
}
myfunc2(9)


# Q3  데이터 비교

myfunc3 = function(data1, data2)
{
  print('mean', myfunc1(mean(data1),mean(data2)))
  print('sd',myfunc1(std(data1),std(data2)))
  print('sum',myfunc1(sum(data1),sum(data2)))
  print('N',myfunc1(length(data1),length(data2)))
}

c(10,23,15,24,15,26) -> data1
c(15,25,16,17,28,34,15,23,74) -> data2

std

myfunc3(data1, data2)




# Q4


# Q5
