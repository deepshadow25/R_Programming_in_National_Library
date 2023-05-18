# 230518

# 국가통계포털 (https://kosis.kr/index/index.do) 데이터 자료 이용
# 데이터 카테고리 -> 인구 -> 인구동향조사 -> 출생 -> 시군구/월별출생

getwd()
setwd("D:/R")

# 데이터 불러오기
install.packages('tidyverse')
install.packages('readxl')

#conflicted
## ... 인줄 알았으나, 데이터의 양이 많아서 오래 걸리는 것이었음
# install.packages("devtools")
# devtools::install_github("r-lib/conflicted")

library(tidyverse)
library(readxl)

read_excel('시군구_성_월별_출생.xlsx') -> birth_df

# 데이터 탐색해 보기
dim(birth_df)
birth_df %>% dim() # dim(birth_df)와 같은 의미

is.na(birth_df)

colSums(is.na(birth_df))
is.na(birth_df$시점)
!is.na(birth_df$시점)

# 1년 중 몇월에 아이들이 가장 많이 태어났을까?
birth_df %>% 
  filter(!is.na(시점)) %>% # -> 시점을 기준으로 필터링
  select(시점, 전국) %>%  # -> 연, 월로 쪼갬
  separate(시점, into = c('년도', '월')) # separate는 . or , 기준으로 데이터를 구분시켜줌
 
 
 



