x=c(NA,1,NA,2)
is.na(x)
mean(x)
mean(x,na.rm=T)
sum(is.na(x))
x[is.na(x)]=mean(x,na.rm=T)
x


x2=rnorm(10000,50,5)
#introduce 30 missing values at random location
length(x2)
posn=sample(100000,size=30)
x2[posn]=NA

sum(is.na(x2))


library(VIM)
head(sleep)
complete.cases(sleep)
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
colSums(is.na(sleep))
rowSums(is.na(sleep))
