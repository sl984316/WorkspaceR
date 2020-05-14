#윤완덕, 20200513/20200513

#Q1
d1 <- 1:50
d2 <- 51:100
#1
d1
d2
#2
length(d2)
#3
d1 + d2 
d2 - d1
d1*d2
d2/d1
#4
sum(d1)
sum(d2)
#5
sum(sum(d1), sum(d2))
#6
max(d2)
min(d2)
#7
mean(d1)
mean(d2)
mean(d2) - mean(d1)
#8
sort(d2, decreasing = TRUE)
#9
sort(d1, decreasing = FALSE)
sort(d2, decreasing = FALSE)
d4 <- sort(d1, decreasing = FALSE)[1:10]
d5 <- sort(d2, decreasing = FALSE)[1:10]
d4
d5
d3 <- c(d4,d5)
d3

#Q2
v1 <- 51:90
#1
v1[v1<60]
#2
sum(v1<70)
#3
sum(v1[v1>65])
#4
v1[v1>60 & v1<73]
#5
v1[v1<65 | v1>80]
#6
v1[v1%%7==3]
#7
v1[v1%%7==0] <- 0
v1

v1 <- 51:90
#8
sum(v1[v1%%2==0])
#9
v1[v1%%2==1 | v1>80]
#10
v1[v1%%3==0 & v1%%5==0]
#11
v1[v1%%2==0] <- v1[v1%%2==0]*2
v1
#12
v1 <- 51:90
v1 <- v1[v1%%7!=0]
v1



