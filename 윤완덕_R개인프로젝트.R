# 윤완덕, R 개인프로젝트

library(dplyr)
library(ggplot2)

setwd('C:\\Workspace\\WorkspaceR')
base <- read.csv('batter_stats_2017.csv', header = T)
base

str(base)

# 문제 지정 : 타자 스탯으로 보는 2017시즌 LG의 부진 이유
# LG는 2017시즌 팀 방어율 4.30으로 전체 1위를 했음에도 불구하고 
# 상위 5팀만이 할 수 있는 포스트시즌에 진출하지 못했다.
# 투수진들은 다른 팀에 비해 좋은 성적을 냈지만, 그럼에도 저조한 순위를 기록한 이유는
# 타자들의 부진을 가장 큰 원인으로 꼽을 수 있다. 
# 2017시즌 타자들의 스탯 기록을 통해 LG가 상위팀으로 갈 수 없었던 이유를 알아보도록 하겠다.


# 1. 명확한 주전 라인업의 부재

# 규정 타석 달성 여부 변수 생성
# 규정 타석 : 리그 전체 경기수 * 3.1 하여 소수점 아래를 버린 값. 타율, 출루율, 장타율 등 비율 스탯의 선수 순위를 매길 때, 
#             규정 타석을 채워야 순위표에 들어갈 수 있다. 한국프로야구는 144경기이므로 규정타석이 되는 타석 수는 446타석이다.


base1 <- base %>% mutate(규정타석=ifelse(타석>=446, 'pass', 'fail') )
base2 <- base1 %>% filter(규정타석=='pass')
base.kia<- base %>% filter(팀명=='KIA')
base.kia
base.ds <- base %>% filter(팀명=='두산')
base.ds
base.lt <- base %>% filter(팀명=='롯데')
base.lt
base.nc<- base %>% filter(팀명=='NC')
base.nc
base.sk <- base %>% filter(팀명=='SK')
base.sk
base.lg <- base %>% filter(팀명=='LG')
base.lg
base.nx<- base %>% filter(팀명=='넥센')
base.nx
base.hw <- base %>% filter(팀명=='한화')
base.hw
base.ss <- base %>% filter(팀명=='삼성')
base.ss
base.kt <- base %>% filter(팀명=='KT')
base.kt



# 팀별 규정타석 달성 선수 수 비교
# 규정타석 달성 선수 수가 많을수록 확실한 주전이 많다는 것을 의미
# 반대로 규정타석 달성 선수 수가 적으면 주전이 자주 바뀌는 것을 의미한다.
dim(base2)
ggplot(base2, aes(x=팀명))+
    geom_bar(fill='lightblue')+
    ggtitle('팀별 규정타석 달성 선수 수')+
    theme(plot.title = element_text(size = 24,
                                    face = 'bold',
                                    colour = 'red'))+
    labs(x='팀 이름', y= '선수 수')

ba1 <- c(8,2,2,5,2,5,3,5,5,2)
rank <- c(1,10,6,4,5,7,2,3,9,8)
df.b <- data.frame(ba1,rank)

cor(ba1,rank)


# 이를 통해 알 수 있 듯 우승팀인 기아의 규정타석 달성 선수는 8명인데 비해, LG의 규정타석 달성 선수는 2명에 불과하다. 
# 이는 확실한 주전이 없어 타팀에 비해 타선의 짜임새가 부족하다고 판단할 수 있다.
# 하지만 규정타석 달성 선수가 LG와 SK가 2명으로 같고, 두산은 3명으로 1명만 많은데 SK와 두산은 모두 포스트시즌 진출에 성공하였다.
# 무슨 차이가 있기에 LG만 포스트시즌 진출에 실패한것인가?


# 2. SK,두산과 LG의 비교 ( 경기당 안타, 홈런, 볼넷, 득점, 타점 )

base.sk<- base %>% filter(팀명=='SK')
base.sk
base.ds <- base %>% filter(팀명=='두산')
base.ds
base.lg <- base %>% filter(팀명=='LG')
base.lg

base.sk.ds.lg<- rbind(base.sk,base.ds,base.lg)


str(base.sk.ds.lg)


options(max.print = 9999999)

# 1) 안타 수 비교
ggplot(base.sk.ds.lg,aes(x=팀명,y=안타))+
    geom_boxplot(fill='orange')+
    ggtitle('LG, SK, 두산의 안타 비교')+
    theme(plot.title = element_text(size = 24,
                                    face='bold'))
base.sk %>% select(안타) %>% sum/144           # 경기 당 안타
base.ds %>% select(안타) %>% sum/144
base.lg %>% select(안타) %>% sum/144

# 안타 측면에서 LG와 두산이 비슷한 양상을 보이고 있다.
# 오히려 경기당 안타는 LG가 두산보다 더 많다.

# 2) 홈런 수 비교
ggplot(base.sk.ds.lg,aes(x=팀명,y=홈런))+
    geom_boxplot(fill='steelblue')+
    ggtitle('LG, SK, 두산의 홈런 비교')+
    theme(plot.title = element_text(size = 24,
                                    face='bold'))
base.sk %>% select(홈런) %>% sum/144           # 경기 당 홈런
base.ds %>% select(홈런) %>% sum/144
base.lg %>% select(홈런) %>% sum/144

# 상자그래프에서 볼 수 있듯이 LG는 SK와 두산에 비해 평균 홈런이 적고,
# 경기당 홈런도 두 팀이 비해 낮음을 알 수 있다.

# 3) 볼넷 수 비교
ggplot(base.sk.ds.lg,aes(x=팀명,y=볼넷))+
    geom_boxplot(fill='pink')+
    ggtitle('LG,SK,두산의 볼넷 비교')+
    theme(plot.title = element_text(size = 24,
                                    face='bold'))

base.sk %>% select(볼넷) %>%sum/144                  # 경기 당 볼넷
base.ds %>% select(볼넷) %>%sum/144
base.lg %>% select(볼넷) %>%sum/144

# 세 팀 간의 볼넷 수 역시 LG가 가장 적은 수치를 나타내고 있다.

# 4) 득점 비교
ggplot(base.sk.ds.lg,aes(x=팀명,y=득점))+
    geom_boxplot(fill='yellow')+
    ggtitle('LG,SK,두산의 득점 비교')+
    theme(plot.title = element_text(size = 24,
                                    face='bold'))

base.sk.ds.lg %>% filter(팀명=='SK') %>% select(득점) %>% sum/144
base.sk.ds.lg %>% filter(팀명=='두산') %>% select(득점) %>% sum/144
base.sk.ds.lg %>% filter(팀명=='LG') %>% select(득점) %>% sum/144

# 5) 타점 비교
ggplot(base.sk.ds.lg,aes(x=팀명,y=타점))+
    geom_boxplot(fill='brown')+
    ggtitle('LG,SK,두산의 타점 비교')+
    theme(plot.title = element_text(size = 24,
                                    face='bold'))
base.sk %>% select(타점) %>%sum/144                  # 경기 당 타점
base.ds %>% select(타점) %>%sum/144
base.lg %>% select(타점) %>%sum/144

# 세 팀 간의 홈런, 볼넷, 득점, 타점을 비교해보면 모든 지표에서 LG가 세 팀 중 가장 낮은 수치를
# 보이고 있다. 경기당 안타는 두산보다 많지만 경기당 타점, 득점이 두산보다 적은 것으로 보아
# LG의 공격이 두산의 공격보다 효율적이지 못하고 안타가 두산에 비해 집중적이지 않음을 알 수 있다.
# SK는 규정타석 달성 선수 수가 적지만 공격 시 효율이 좋아 LG보다 좋은 성적을 낼 수 있었다.
# 이는 규정타석 달성 선수 수가 비슷한 세 팀이지만 LG만 포스트시즌에 가지 못한 이유이다.



# 3. 팀 순위별 WAR ( 대체 선수 대비 승리 기여도 ) 합 비교
sum(base.lg$WAR)
war <- c(sum(base.kia$WAR),sum(base.ds$WAR),sum(base.lt$WAR),sum(base.nc$WAR),sum(base.sk$WAR),
         sum(base.lg$WAR),sum(base.nx$WAR),sum(base.hw$WAR),sum(base.ss$WAR),sum(base.kt$WAR))
war
team <- c(1,2,3,4,5,6,7,8,9,10)
names(team) <- c('KIA','두산','롯데','NC','SK','LG','넥센','한화','삼성','KT')

df.base <- data.frame(team,war)
df.base

ggplot(df.base,aes(x=team, y=war))+
    geom_bar(stat = 'identity',       
             width = 0.5,             
             fill='steelblue') + 
    ggtitle('팀 순위 별 합산 WAR')+
    theme(plot.title = element_text(size = 24,
                                    face = 'bold',
                                    colour = 'red'))+
    labs(x="팀 순위", y= 'WAR')

cor(war,team)   # 등수와 war의 상관계수가 -0.7640916 매우 강한 상관 관계가 있음을 알 수 있다.

# 팀별로 각 선수들의 WAR의 합을 비교해본 결과 6위 LG는 신생팀인 10위 KT를 제외하고 최하위에 위치하고 있다.
# 1위 KIA의 경우 WAR의 합이 30이 넘는데, 이는 KIA 선수들이 대체 선수에 비해 30승을 더 챙겼다는 의미다.
# 반면 LG 선수들의 WAR 합은 6.8이므로 대체 선수에 비해 6~7승 정도만 더 챙겼음을 의미한다.
# 이는 LG 선수들이 타 팀 선수들에 비해 승리에 기여하는 정도가 전체적으로 낮다고 할 수 있다.



# 4. 피타고리안 승률과 실제 순위 비교

# 피타고리안 승률이란 득점의 제곱을 득점의 제곱과 실점의 제곱의 합으로 나눈 수로 
# 팀이 얻은 득점과 잃은 실점으로 승률을 예상하는 용도로 쓰인다.

team
runs.hit <- c(906,849,743,786,761,699,789,737,757,655)
runs.pitch <- c(734,678,701,745,767,677,764,820,911,876)



runs<- data.frame(team,runs.hit,runs.pitch)
runs


runs1<- runs %>% mutate(피타고리안_승률=runs.hit^2/(runs.hit^2+runs.pitch^2))
runs1


ggplot(runs1,aes(x=team,y=runs.pitch))+
    geom_bar(stat = 'identity',       
             width = 0.5,             
             fill='steelblue')+
    ggtitle('순위 별 팀 실점')+
    theme(plot.title = element_text(size = 24,
                                    face = 'bold',
                                    colour = 'red'))+
    labs(x='팀 순위', y='실점')

ggplot(runs1,aes(x=team,y=runs.hit))+
    geom_bar(stat = 'identity',       
             width = 0.5,             
             fill='steelblue')+
    ggtitle('순위 별 팀 득점')+
    theme(plot.title = element_text(size = 24,
                                    face = 'bold',
                                    colour = 'red'))+
    labs(x='팀 순위', y='득점')
              
ggplot(runs1,aes(x=team,y=피타고리안_승률))+
    geom_bar(stat = 'identity',       
             width = 0.5,             
             fill='steelblue')+
    ggtitle('순위 별 피타고리안 승률')+
    theme(plot.title = element_text(size = 24,
                                    face = 'bold',
                                    colour = 'red'))+
    labs(x='팀 순위', y='피타고리안 승률')


cor(runs1)

corrplot(cor(runs1),method = "number")


# LG는 피타고리안 승률이 0.515로 5위이지만, 실제승률은 0.489로 
# 피타고리안 승률보다 낮은 수치를 기록하였다. 
# 이는 실점은 적지만 득점이 다른 경쟁 팀에 비해 매우 떨어져서 나온 결과라 할 수 있다. 





