#윤완덕 20200514

#Q1
#1
score <- matrix(c(10,40,60,20,21,60,70,30),4,2)
score
#2 
colnames(score) <- c('male','female')
score
#3
score[2,]
#4
score[,'female']
#5
score[3,2]

#Q2
#1
st <- data.frame(state.x77)
#2
st
#3
colnames(st)
#4
rownames(st)
#5
nrow(st)
ncol(st)
#6
str(st)
#7
apply(st,1,sum)
apply(st,1,mean)
#8
apply(st,2,sum)
apply(st,2,mean)
#9
st['Florida',]
#10
st[,'Income']
#11
st['Texas','Area']
#12
st['Ohio',c('Population','Income')]
#13
subset(st,Population>=5000)
#14
subset(st,Income>=4500)[,c('Population','Income','Area')]
#15
nrow(subset(st,Income>=4500))
#16
subset(st,Area>=100000 & Frost>=120)
#17
subset(st,Population<2000 & Murder<12)
#18
mean(subset(st,Illiteracy>=2.0)[,'Income'])
#19
abs(mean(subset(st,Illiteracy>=2.0)[,'Income'])-mean(subset(st,Illiteracy<2.0)[,'Income']))
#20
max(st$Life.Exp)
rownames(subset(st,Life.Exp == max(st$Life.Exp)))
#21
st['Pennsylvania', 'Income']
rownames(subset(st,Income > st['Pennsylvania','Income']))

sum(st[,'Population'])
sum(st[, 'Income'])
st$Income
st[,'Income']


#Q3
#1
mtcars
class(mtcars)
#2
nrow(mtcars)
ncol(mtcars)
#3
str(mtcars)
#4

#5

#6
mtcars['Honda Civic',c('mpg','gear')]
#7

#8
mean(mtcars[,1])
#9





