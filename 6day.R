#
# 2.1 다중 변수 자료 탐색
#
# - 다중 변수 ( 다변량 ) 자료는 변수가 2개 이상인 자료
# - 다중 변수 자료는 2차원 형태, matrix나 data frame에 저장하여 분석
# - 다중 변수 자료에서 변수는 열( column ), 개별 관측값들이 행( row )으로 구성
#                                           
# 산점도 ( scatter plot ) 
# : 2개의 변수로 구성된 자료의 분포를 알아보는 그래프.
#   관측값들의 분포를 2개의 변수 사이의 관계를 파악할 수 있는 기법
#
# - 산점도는 두 변수의 데이터 분포를 나타내는 것이기 때문에 두 개의 변수에 대한 자료가 필요
# - 산점도는 관측값들의 분포를 보면서 두 변수 사이의 관련성을 확인하는 데 사용



# 두 변수 ( 이변량 )에 다한 산점도
str(mtcars)
wt <- mtcars$wt
wt
mpg <- mtcars$mpg
mpg

plot(wt,mpg,
     main = '중량-연비 그래프',
     xlab='중량',
     ylab='연비',
     col= 'red',
     pch=20)


plot(mtcars$wt,mtcars$mpg,
     main = '중량-연비 그래프',
     xlab='중량',
     ylab='연비',
     col= 'red',
     pch=20)


plot(mtcars[,c('wt','mpg')],
     main = '중량-연비 그래프',
     xlab='중량',
     ylab='연비',
     col= 'red',
     pch=20)


plot(mpg~wt,data=mtcars,
     main = '중량-연비 그래프',
     xlab='중량',
     ylab='연비',
     col= 'red',
     pch=20)

# 2개 이상의 변수 ( 다변량 )에 대한 산점도
vars <- c('mpg','disp','drat','wt')
vars
target <- mtcars[,vars]
target
head(target)

pairs(target,main='multi plots', pch=20)

plot(target,main='multi plots')

# - pairs()는 여러 개의 변수에 대해 짝지어진 산점도 작성
# - 다중 산점도는 대각선을 기준으로 오른쪽 위의 산점도들과 왼쪽 아래의 산점도들이 대칭을 이룬다
#   (동일한 산점도인데 x축과 y축이 바뀌어 있다)
# - 다중 산점도는 여러 변수들 간의 추세를 한눈에 파악할 수 있다.
#
# - 두 개의 변수에 대한 산점도를 작성할 때, 그룹 정보를 알고 있다면 작성 시  
#   각 그룹 별 관측값들에 대해 서로 다른 색과 점의 모양으로 표시 할 수 있다.
#   두 변수 간의 관계 뿐만 아니라 그룹 간의 관계도 파악할 수 있다.
#
iris.2 <- iris[,3:4]
point <- as.numeric(iris$Species)
point

color <- c('red','green','blue')

plot(iris.2,main='Iris plot',
     pch=c(point),
     col=color[point])

# 2.2 상관분석
#
# - 두 변수 간의 관계를 분석 시 추세의 모양이 선의 형태를 갖는 것을
#   선형적 관계라 표현한다.
# 
# - 선형적 관계는 강한 선형적 관계와 약한 선형적 관계가 있다.
#
# - 상관 분석( correlation analysis )
#  : 얼마나 선형성을 보이는 지 수치상으로 나타내는 방법
# 
# - 상관 계수( correlation coefficient )는 선형성의 정도를 나타내는 척도
# 
# - 상관 계수 r의 특징
#   -1 <= r <= 1
#   r>0 : 양의 상관 관계 ( x가 증가하면 y도 증가 )
#   r<0 : 음의 상관 관계 ( x가 증가하면 y는 감소 )
#   r이 1이나 -1에 가까울수록 x,y의 상관성이 높고, 
#   관측값들의 분포가 직선에 가까워진다.
beers <-  c(5,2,9,8,3,7,3,5,3,5)
bal <-  c(0.1,0.03,0.19,0.12,0.04,0.0095,0.07,0.06,0.02,0.05)
tbl <- data.frame(beers,bal)
tbl

plot(bal~beers,data=tbl, pch = 20)
res <- lm(bal~beers,data=tbl)  # 회귀식 도출
abline(res)                    # 회귀선 그리기
cor(beers,bal)                 # 상관 계수 계산


par(mfrow=c(1,2))

plot(wt~mpg, data = mtcars,
     main='중량-연비 그래프',
     xlab='연비(MPG)',
     ylab='중량',
     col='orange',
     pch=20)

res <- lm(wt~mpg, data = mtcars)
abline(res)


plot(drat~wt, data=mtcars,
     main='리어액슬기어비-중량 그래프',
     xlab='중량',
     ylab='리어액슬기어비',
     col='blue',
     pch=20)

res2 <- lm(drat~wt, data=mtcars)
abline(res2)

cor(mtcars$wt,mtcars$mpg)
cor(mtcars$drat,mtcars$wt)


cor(iris[,1:4])

par(mfrow=c(1,1))


# 시계열 자료 ( time series data ) : 시간의 변화에 따라 수집한 자료
# 
# 선그래프 : 다중 변수 자료의 변수 중 하나가 연월과 같이 시간을 나타내는 값을 갖는 경우,
#            x축을 시간축으로 하여 시간의 변화에 따른 자료의 증감 추이를 확인할 때 사용
month <- 1:12
late <- c(5,8,7,9,4,6,12,13,8,6,6,4)
late2 <- c(4,6,5,8,7,8,10,11,6,5,7,3)
plot (month,                    # x 변수
      late,                     # y 변수
      main = '지각생 통계',     # 제목
      type='o',                 # 그래프 종류 ( l,b,s,o )
      lty=1,                    # 선의 종류 
      lwd=1,                    # 선의 굵기
      col='red',
      ylim = c(1,15),
      xlab='Month',             # x축 이름
      ylab='Late cnt')          # y축 이름

lines (month,late2,type='b',col='blue')


#
# 2.3 자료 탐색 실습 ( EDA )
#

# 1. 분석 대상 데이터셋 준비
#

# Boston Housing 데이터셋 활용
install.packages("mlbench")

library(mlbench)
data("BostonHousing")
myds <- BostonHousing[,c('crim','rm','dis','tax','medv')]

myds
class(myds)
dim(myds)
str(myds)
head(myds)

#
# 2. 필요한 변수 추가
#
# crim : 지역 1인당 범죄율, rm : 주택 1가구당 방의 개수
# dis : 보스턴 5개 직업 센터까지 거리
# tax : 재산세율, medv : 주택가격
grp <- c()
for (i in 1:nrow(myds)){
        if(myds$medv[i]>=25.0){
                grp[i] <- 'H'
        } else if (myds$medv[i] <= 17.0){
                grp[i] <- 'L'
        } else {
                grp[i] <- 'M'
        }
}

grp <- factor(grp)
grp <- factor(grp, levels=c('H','M','L'))

myds <- data.frame(myds,grp)


# 3. 데이터셋 형태와 기본적인 내용 파악
#
str(myds)
head(myds)
table(grp)

# 4. 히스토그램을 이용한 관측값 분포 확인
#
par(mfrow = c(2,3))
for(i in 1:5){
        hist(myds[,i], main = colnames(myds)[i],
             col = 'yellow')
}

par(mfrow=c(1,1))

#
# 5. 상자 그래프에 의한 관측값 분포 확인
#
par(mfrow=c(2,3))
for(i in 1:5){
        boxplot(myds[,i],main = colnames(myds)[i],
                col = 'orange')
}

par(mfrow=c(1,1))


#
# 6. 그룹별 관측값 분포 확인
#
boxplot(myds$crim~myds$grp.1, main= ' 1인당 범죄율')
boxplot(myds$rm~myds$grp.1, main= ' 방의 개수')


#
# 7. 다중 산점도를 이용한 변수 간 상관 관계 확인
#
pairs(myds[,1:5])


#
# 8. 그룹 정보를 포함한 변수 간 상관 관계
#a
point <- as.integer(myds$grp.1)
color <- c('orange','skyblue','yellow')
pairs ( myds[,1:5],pch=point,
        col=color[point])

#
# 9. 변수 간 상관 계수 확인
#
cor (myds[,1:5])






#
# 도로교통공단_시도_시군구별_월별_교통사고(2018)을 이용한 EDA 실습
#
setwd("C:\\Workspace\\WorkspaceR")
acd <- read.csv('도로교통공단_시도_시군구별_월별_교통사고(2018).csv',header=T)
acd

str(acd)
head(acd)
tail(acd)

month <- as.factor(acd$월)
month


pairs(acd[,4:9])  # 발생건수와 사망자수는 상관관계가 낮고, 발생건수와 나머지와의 상관관계는 높다.
                  # 사망자수는 다른 모든 변수들과 상관관계가 낮다.
cor(acd[,4:9])

mon <- c('01월','02월','03월','04월','05월','06월','07월','08월','09월','10월','11월','12월')

sum(acd$사망자수[month=='01월'])
sum(acd$사망자수[month=='12월'])

area <- as.factor(acd$시도)
area

acd1 <- acd[,c('발생건수','사망자수','부상자수','중상','경상','부상신고')]
acd1 

par(mfrow=c(3,3))
for(i in 1:6){
        hist(acd1[,i], main = colnames(acd1)[i],
             col = 'yellow')
}


































