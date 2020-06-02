setwd('C:\\Workspace\\WorkspaceR\\project2')


# 노인 인구수 연도별 추이
total <- read.csv('total.csv', header = T)
total
colnames(total) <- c('구역', '연령','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')
total
total<- total[-1,]
total<- total[,-1]


total$`2009`<- as.numeric(total$`2009`)
total$`2010`<- as.numeric(total$`2010`)
total$`2011`<- as.numeric(total$`2011`)
total$`2012`<- as.numeric(total$`2012`)
total$`2013`<- as.numeric(total$`2013`)
total$`2014` <- as.numeric(total$`2014`)
total$`2015`<- as.numeric(total$`2015`)
total$`2016`<- as.numeric(total$`2016`)
total$`2017`<- as.numeric(total$`2017`)
total$`2018`<- as.numeric(total$`2018`)
total$`2019`<- as.numeric(total$`2019`)

str(total)

total[38,] <- c('65세 이상', sum(total$`2009`),sum(total$`2010`),sum(total$`2011`),sum(total$`2012`),sum(total$`2013`),sum(total$`2014`),sum(total$`2015`),sum(total$`2016`),sum(total$`2017`),sum(total$`2018`),sum(total$`2019`))
sum(total$`2009년`)

total<- t(total[38,])


total <- as.data.frame(total)



colnames(total) <- NULL

str(total)

total<- total[-1,]

library(dplyr)
library(ggplot2)

colnames(total) <- c('65세_이상')


year<- rownames(total)
old65 <- total[,1]

total.old <- data.frame(year,old65)


total.old$year <- as.numeric(total.old$year)
total.old$old65 <- as.numeric(total.old$old65)

ggplot(total.old, aes(x=year,y=old65))+
    geom_line(linetype=1, size=0.7, col="darkgreen")+
    geom_point(size=2, shape=19,col='reD')+
    ggtitle('최근 10년 간 노인 수 변동')+
    scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
    theme_classic()+
    labs(x="연도", y="65세 이상 인구 수")+
    theme(plot.title = element_text(size=25,
                                    face='bold',
                                    colour = 'steelblue'))
  


# 노인복지시설 연도별 추이

#노인주거복지시설 : 일상생활에 지장이 없는 노인이 입소하여 일상생활에 필요한 편의를 제공받는 시설로 양로시설·노인공동생활가정·노인복지주택을 말함(노인복지법 제32조)
#노인의료복지시설 : 노인성 질환 등으로 요양을 필요로하는 노인이 입소하여 급식·요양 등 일상생활에 필요한 편의를 제공받는 시설로 노인요양시설·노인요양공동생활가정을 말함(노인복지법 제34조)
#노인여가복지시설 : 노인들이 친목도모 및 취미활동 등 여가활동을 할 수 있는 시설로 노인복지관·경로당·노인교실이 있음(노인복지법 제36조)
#재가노인복지시설 : 신체·정신적 장애로 일상생활을 영위하는데 다른 사람의 도움이 필요한 노인에게 각종 편의를 제공하는 시설로 방문요양·주야간보호·단기보호·방문목욕·방문간호·복지용구지원서비스·재가노인지원서비스가 있음(노인복지법 제38조)

happy <- read.csv('oldplace.csv')
happy<- happy[-1,]
happy <- happy[c(1,2,6,9,13),]    
happy <- happy[,-2]
happy<- t(happy)
colnames(happy) <- NULL
rownames(happy) <- NULL
happy<- happy[-1,]
happy<- happy[4:13,]
happy <- as.data.frame(happy)

colnames(happy) <- c('year','노인주거복지시설', '노인의료복지시설','노인여가복지시설','재가노인복지시설')

happy$year <- as.numeric(happy$year)
happy$노인주거복지시설 <- as.numeric(happy$노인주거복지시설)
happy$노인의료복지시설<- gsub(',','',happy$노인의료복지시설)
happy$노인의료복지시설 <- as.numeric(happy$노인의료복지시설)
happy$노인여가복지시설<- gsub(',','',happy$노인여가복지시설)
happy$노인여가복지시설 <- as.numeric(happy$노인여가복지시설)
happy$재가노인복지시설<- gsub(',','',happy$재가노인복지시설)
happy$재가노인복지시설 <- as.numeric(happy$재가노인복지시설)



happy65<- merge(total.old, happy, by=c('year'))
happy65$old65 <- happy65$old65/1000
happy65$노인여가복지시설 <- happy65$노인여가복지시설/10

# 선그래프 그리기

happy주거<- happy65[,c(1,3)]

ggplot(happy주거, aes(x=year,y=노인주거복지시설))+
  geom_line(linetype=1, size=0.7, col="darkgreen")+
  geom_point(size=2, shape=19,col='reD')+
  ggtitle('10년간 노인 주거복지시설 수 추이')+
  scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  theme_classic()+
  labs(x="연도", y="노인주거복지시설 수")+
  theme(plot.title = element_text(size=25,
                                  face='bold',
                                  colour = 'steelblue'))

happy1<- data.frame(happy65$year,happy65$old65,c('노인_수','노인_수','노인_수','노인_수','노인_수','노인_수','노인_수','노인_수','노인_수','노인_수'))
happy2 <- data.frame(happy65$year,happy65$노인의료복지시설,c('노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설','노인의료복지시설'))
happy3 <- data.frame(happy65$year,happy65$노인여가복지시설,c('노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설','노인여가복지시설'))
happy4 <- data.frame(happy65$year,happy65$재가노인복지시설,c('재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설','재가노인복지시설'))

colnames(happy1) <- c('연도','수','그룹')
colnames(happy2) <- c('연도','수','그룹')
colnames(happy3) <- c('연도','수','그룹')
colnames(happy4) <- c('연도','수','그룹')

happy5<- rbind(happy1,happy2,happy3,happy4)

happy5$그룹 <- as.factor(happy5$그룹)



ggplot(happy5,aes(x=연도, y=수,color=그룹))+
  geom_line(size=2)+
  geom_point(size=2, shape=19,col='black')+
  ggtitle('연도별 노인 수와 종류별 노인복지시설 추이')+
  theme_classic()+
  theme(plot.title = element_text(size=25,
                                  face='bold',
                                  colour = 'steelblue'))
  


# 노인 수는 단위 1000명, 여가복지시설은 단위 10개







