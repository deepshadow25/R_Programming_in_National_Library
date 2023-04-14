# 230414
# 4월 13일 수업에 이어서 진행

# salary_mid, sales

DATA_mid = subset(DATA, salary == 'medium')
DATA_sales = subset(DATA, sales =='sales')

DATA_Mid_Sales = subset(DATA, salary == 'medium' & sales == 'sales')
remove(DATA_Mid_Sales)


# 집계된 데이터를 생성

install.packages('plyr')
library(plyr)

ss = ddply()
