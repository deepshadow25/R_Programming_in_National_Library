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
