# 230414

# salary_mid, sales




DATA_mid = subset(DATA, salary == 'medium')
DATA_sales = subset(DATA, sales =='sales')

DATA_Mid_Sales = subset(DATA, salary == 'medium' & sales == 'sales')
remove(DATA_Mid_Sales)


# 집계된 데이터를 생성

install.packages('plyr')
library(plyr)

ss = ddply(DATA, c('sales','salary'), summarise,            
           M_SF = mean(satisfaction_level),                
           COUNT = length(sales)),                       
           M_Wh = round(mean(average_montly_hours), 2)   
ss = ddply(DATA, c('sales','salary'), summarise,            # sales, salary 별로 요약값들을 계산
          M_SF = mean(satisfaction_level),                  # satisfaction_level의 평균 계산            
           COUNT = length(sales),                           # sales, salary 별로 직원수 카운팅        
            M_Wh = round(mean(average_montly_hours), 2))    # average_montly_hours 평균 계산

# Heatmap 그리기

ggplot(ss) + # ss데이터 지정
  geom_tile(aes(x = sales, y = salary, fill = M_Wh)) + # x축은 sales, y축은 salary, 색은 평균 근무시간으로 설정
  scale_fill_gradientn(colours = c('#eeeeee', '#020831'), values = c(0,0.8,1))

# 데이터 요약
summary(DATA$salary)
DATA$salary = as.numeric(DATA$salary)
summary(DATA$salary)
summary(DATA$satisfaction_level)


# 연습문제 1

Work_period = ifelse(DATA$time_spend_company > 5, 'Expert',
                     ifelse(DATA$time_spend_company > 2, 'Middle', 'New'))

# 연습문제 2

summary(DATA$average_montly_hours)

Working_Time = ifelse(DATA$average_montly_hours > 245.0, 'Very Busy',
                      ifelse(DATA$average_montly_hours > 200.0, 'Busy',
                             ifelse(DATA$average_montly_hours > 156.0, 'Free', 'Very Free')))


# 연습문제 3

Work_mid_busy = subset(DATA, Work_period == 'Middle' & (Working_Time == 'Busy' | Working_Time == 'Very Busy'))


# 연습문제 4

Work = ddply(DATA, c('Work_period', 'Working_Time'), summarise,
             M_projects = mean(DATA$number_project),
             M_satisfaction_level = mean(DATA$satisfaction_level),
             M_last_evalutation = mean(DATA$last_evaluation))

Work

# 연습문제 5

DATA$time_spend_company = as.factor(DATA$time_spend_company)
DATA$time_spend_company  

ggplot(DATA, aes(x = left)) + geom_bar(aes(fill = DATA$time_spend_company)) + xlab('이직 여부') + labs(fill = '근속년수')

ggplot(DATA, aes(x = satisfaction_level)) + geom_density(aes(fill=left, alpha = 0.3)) + labs(fill='이직여부')
