# 230519

getwd()
setwd("D:/R")
getwd()

# 타이타닉호 힘몰 사건 케이스 스터디
# 생존에 "운"적인 요소도 존재하지만, 일부 그룹은 다른 그룹보다 더 많이 생존
# 이번 분석 과제에서 이름, 나이,성별,사회/경제적 계층이라는 변수를 활용
# 더 높은 생존율을 가진 모형을 구축

# 가설 설정
# 아이, 노인의 생존율이 높을 것이다.
# 여성이 남성보다 생존율이 높을 것이다.
# 사회/경제적 계층이 높은 사람이 낮은 사람보다 생존율이 높을 것이다.

# 라이브러리 호출
install.packages("corrplot")
install.packages("randomForest")
install.packages("ggthemes")
install.packages("plyr")
install.packages("caret")

library(tidyverse)
library(corrplot)
library(randomForest)
library(ggthemes)
library(scales)
library(plyr)
library(caret)

train <- read_csv("./train.csv")
test <- read_csv("./test.csv")
full <- bind_rows(train,test)

str(full)
summary(full)
glimpse(full)

# 데이터들을 간단히 살펴보자.
# str() 함수를 통해 PassengeId, Survived, Pclass, Age, SibSp, Parch, Ticket, Fare는 숫자 확인
# Name, Sex, Ticket, Cabin, Embarked는 글자로 이루어진 Charachter
# Survived는 살아 있다면 1, 사망 했다면 0 Numeric(숫자)으로 되어 있는 만큼 범주화 변경 필요

sort(colSums(is.na(full)), decreasing = T)

# 결측치 데이터 처리

### AGE
full %>%
  filter(is.na(Age) & Sex == 'male') %>%

head()

full[1:891,] %>%
  filter(!is.na(Age) & Sex == "male") %>%
  ggplot(aes(x = Age, fill = factor(Pclass))) +
  geom_density(position = "identity", alpha = 0.4, na.rm = TRUE) +
  geom_vline(aes(xintercept=median(Age[Pclass == 3],na.rm = TRUE)), color = 'Dark Blue', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 2],na.rm = TRUE)), color = 'Dark Green', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 1],na.rm = TRUE)), color = 'Tomato', linetype = 'dashed') +
  theme(legend.position = c(0.9,0.9),
        legend.title  = element_blank()) +
  labs(title = "Distribution of male Age By Pclass")


full[1:891,] %>%
  filter(!is.na(Age) & Sex == "female") %>%
  ggplot(aes(x = Age, fill = factor(Pclass))) +
  geom_density(position = "identity", alpha = 0.4, na.rm = TRUE) +
  geom_vline(aes(xintercept=median(Age[Pclass == 3],na.rm = TRUE)), color = 'Dark Blue', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 2],na.rm = TRUE)), color = 'Dark Green', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 1],na.rm = TRUE)), color = 'Tomato', linetype = 'dashed') +
  theme(legend.position = c(0.9,0.9),
        legend.title  = element_blank()) +
  labs(title = "Distribution of Female Age By Pclass")

# Pclass에 따라 연령의 차이가 존재, 연령이 높을 수록 경제적 여유가 있기 때문에 더 좋은 좌석을 구매
for (i in 1:3){
  full[is.na(full$Age) & full$Pclass == i & full$Sex == 'male',]$Age <- median(full[full$Sex == 'male' & full$Pclass == i,]$Age, na.rm = TRUE)
}

for (i in 1:3){
  full[is.na(full$Age) & full$Pclass == i & full$Sex == 'female',]$Age <- median(full[full$Sex == 'female' & full$Pclass == i,]$Age, na.rm = TRUE)
}

# 결측치에 Pclass에 해당되는 연령의 중앙값을 대입

### EMBARKED
full %>%
  filter(is.na(Embarked))

table(full$Embarked)
full$Embarked[c(62,830)] <- "S" # 가장 많은 S를 대입

### FARE
full %>%
  filter(is.na(Fare))

ggplot(data = full[full$Pclass == 3,], aes(x = Fare)) +
  geom_density(alpha = 0.5, fill = "sky blue", na.rm = TRUE) +
  geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)), linetype = "dashed", lwd = 1, colour = "Royal Blue") +
  labs(title = "Density Pclass = 3",
       x = "Fare",
       y = "Density")

full$Fare[1044] <- median(full$Fare[full$Pclass == 3], na.rm =TRUE) # Pclass가 3인 운임료의 중앙값을 대입

# 결측치 재확인
sort(colSums(is.na(full)), decreasing = TRUE)

# 시각화

# 연령
ggplot(data =full[1:819,], aes(x = Age, fill = factor(Survived))) +
  geom_density(position = "identity",alpha = 0.5, na.rm = TRUE) + # alpha는 투명도, na.rm = TRUE는 결측값 삭제 처리.
  labs(title = "Age vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) + # legend의 이름을 바꿔줌.
  theme(legend.position = c(0.9,0.9)) # legend의 위치를 옮겨줌
# 10세 이하는 많이 살아 남았지만, 청장년층과 노인은 사망자가 더 많은 경향을 보임

# 성별
ggplot(data= full[1:819,], aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  labs(title = "Sex vs Survied") +
  scale_fill_discrete(name = element_text("Survived")) +
  theme(legend.position = c(0.9,0.9))

# 가족
full$Family <- full$SibSp + full$Parch + 1

ggplot(data = full[1:891,], aes(x = Family, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Family Size vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) +
  scale_x_continuous(breaks = seq(1,11,1)) + # xline의 범위를 정해줌.
  theme(legend.position = c(0.9,0.9))

full$FamD[full$Family == 1] <- "Alone"
full$FamD[full$Family < 4 & full$Family > 1] <- "Small"
full$FamD[full$Family >= 4] <- "Big"

mosaicplot(table(full$FamD, full$Survived), main = "Mosaic Plot of Family Size", xlab = "Family size", ylab = "Survived", col = hcl(c(50,120)),)

# 이름 (성별을 잠재적으로 나타내는 Mr, Miss 등 Name이라는 변수를 통해 파생변수를 만들어 분석에 활용)
title <- full$Name
title <- gsub("^.*, (.*?)\\..*$", "\\1", title) # 정규표현식

full$title <- title
unique(full$title)

sort(table(full$title), decreasing = T)

full[title %in% c("Capt","Col","Major","Dr","Rev","Don","Sir","the Countess","Jonkheer"),]$title <- "Officer"
full[title %in% c("Mlle", "Ms", "Lady","Dona"),]$title <- "Miss"
full[title == "Mme",]$title <- "Mrs"

full$title <- as.factor(full$title)
# Mr, Miss, Mrs, Master, Officer로 총 5가지로 범주화
ggplot(data = full[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(name = element_text("Survived")) +
  theme(legend.position = c(0.9,0.9))

# 어린이 (18세 미만은 child로 18세 이상은 adult로 분리)
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"

df <- full[1:891,]

ggplot(data = df[df$Child == "Child",], aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) +
  labs(title = "Pclass vs Sex vs Child vs Survived") +
  theme(legend.title = element_blank())

# corr Heat Map 상관관계 확인
corr.data <- full[1:891,]

corr.data$Embarked <- revalue(corr.data$Embarked, c("S" = 1, "Q" = 2, "C" = 3))
corr.data$Sex <- revalue(corr.data$Sex, c("male" =1, "female" = 2))
corr.data$FamD <- revalue(corr.data$FamD, c("Alone" = 1, "Small" = 2, "Big" = 3))
corr.data$Child <- revalue(corr.data$Child, c("Child" = 1, "Adult" = 2))
corr.data$title <- revalue(corr.data$title, c("Miss" = 1, "Mrs" = 2, "Master" = 3, "Officer" = 4, "Mr" = 5))

corr.data$Child <- as.numeric(corr.data$Child)
corr.data$Sex <- as.numeric(corr.data$Sex)
corr.data$Embarked <- as.numeric(corr.data$Embarked)
corr.data$FamD <- as.numeric(corr.data$FamD)
corr.data$Pclass <- as.numeric(corr.data$Pclass)
corr.data$Survived <- as.numeric(corr.data$Survived)
corr.data$title <- as.numeric(corr.data$title)

corr.data <- corr.data[,c("Child", "Sex", "Embarked", "FamD","Pclass", "Fare", "Age", "title", "Survived")]
str(corr.data)
corr.data<-cor(corr.data)

corrplot.mixed(corr.data, tl.pos = "lt", tl.col = 'black')

full$Child  <- factor(full$Child)
full$Sex  <- factor(full$Sex)
full$Embarked  <- factor(full$Embarked)
full$Pclass  <- factor(full$Pclass)
full$FamD  <- factor(full$FamD)
full$title <- factor(full$title)

colnames(full)

# 사용하지 않는 ticket과 cabin 제거
full.no.ticket <- full[,-9]
full_no.cabin <- full.no.ticket[,-10]

train <- full_no.cabin[1:891,]
test <- full_no.cabin[892:1309,]

set.seed(1234)
model <- randomForest(factor(Survived) ~ Child + Sex + Embarked + FamD + Pclass + Fare + title, data =train, importance = T, ntree = 500)
fitted <- predict(model)
print(model)

# Confusion Matrix를 보면 사망자 중 사망했다고 예측한 것은 506, 생존했다고 예측한 것은 43명입니다.
# 생존자 중에서 사망했다고 예측한 것은 109, 생존했다고 예측한 것은 233명입니다. 정확도는 82.8%입니다.

importance(model)
varImpPlot(model)

prediction <- predict(model, test)
solution <- data.frame(Survived = prediction, PassengerID = test$PassengerId)
write.csv(solution, file = "230519_model.solution.csv", row.names = F)

# 결론
# 1. 아이의 경우 생존율이 높음 (노인과 크게 차이가 없음)
# 2. 여성은 남성보다 높은 생존율
# 3. 사회경제적 계층이 높은 사람은 낮은 사람보다 생존율이 높은
