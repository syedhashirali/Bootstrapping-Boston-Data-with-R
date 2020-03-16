
# Exam 2: code set 2
#
# Execute with Microsoft R Open 3.5.1 on Windows
#
###

options(scipen=10)

require(tidyverse)
require(broom)
require(data.table)
require(MASS)
#read data
data(Boston)
#get complete cases
raw    <- Boston[complete.cases(Boston),]
#nr is number of rows same as before
nr     <- nrow(raw)
#all are numbers except for rad 
str(raw)

seed   <- 428088851
set.seed(seed)
#select medv as y and all others as x call it mdl
mdl    <- medv~.
#see the coefficients, intervals and p values for base model train on all rows of raw
summary(lm(mdl,raw))

#sample 506 rows 500 times with replacement
# t1 is a datatable with 2 cols. i is the numbers 1 to 500 repeated nr times or 506 times
#j is a shuffled replication with replacements of digits 1 to 506 in each row, 500 times 
t1     <- data.table(i=rep(1:500,each=nr),
                     j=as.vector(replicate(500,sample(nr,replace=T),simplify=T)))
#find coefficients for classic bootstrap
#for all rows in t1 apply lm to all rows included in j (shuffled sampled with replacement), group them by i or bootstrap number 1 to 500 
# tidy the results
r1     <- t1[,tidy(lm(mdl,raw[j,])),by=i]
head(t1)
#r1
alpha         <- 0.05
cl.gb<-r1[,.(m.est=mean(estimate),   sd.est=sd(estimate),n.l95=quantile(estimate,alpha/2),n.u95=quantile(estimate,1-(alpha/2)),
      n.l90=quantile(estimate,alpha), n.u90=quantile(estimate,1-alpha), n.50 =quantile(estimate,0.50), pc.alpha=sum(p.value<=alpha)/length(p.value) ), by=term]
t<- 1.96*cl.gb$sd.est 
cl.gb$p.l95   <- cl.gb$m.est-t
cl.gb$p.u95   <- cl.gb$m.est+t 
t<- 1.645*cl.gb$sd.est 
cl.gb$p.l90<- cl.gb$m.est-t 
cl.gb$p.u90   <- cl.gb$m.est+t

#cl.gb
View(cl.gb)

set.seed(seed)
#t2 is a data.table with i as a shuffled repetetion of digits 1 to 506, 500 times
#and j is a repetetion of 1 to 500 each 506 times (just like i in the previous model)
t2     <- data.table(i=sample(rep(1:nr,500)),j=rep(1:500,each=nr))
head(t2)
r2     <- t2[,tidy(lm(mdl,raw[i,])),by=j]

bal.gb<- r2[,.(m.est=mean(estimate), sd.est=sd(estimate),n.l95=quantile(estimate,alpha/2), n.u95=quantile(estimate,1-(alpha/2)), n.l90=quantile(estimate,alpha), n.u90=quantile(estimate,1-alpha),  n.50 =quantile(estimate,0.50), pc.alpha=sum(p.value<=alpha)/length(p.value) ), by=term] 
t <- 1.96*bal.gb$sd.est 
bal.gb$p.l95  <- bal.gb$m.est-t 
bal.gb$p.u95  <- bal.gb$m.est+t 

t<- 1.645*bal.gb$sd.est
bal.gb$p.l90  <- bal.gb$m.est-t 
bal.gb$p.u90  <- bal.gb$m.est+t 
View(bal.gb)




















