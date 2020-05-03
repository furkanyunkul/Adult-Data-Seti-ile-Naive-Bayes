getwd()
setwd("D:\\dataMinig\\r\\udemy\\temel\\NaiveBayes")
data=read.csv("adult.csv")
str(data)
#na degerlerinin toplami
apply(data,2,function(x){sum(is.na(x))})
#na degeri sifir cikti
#verideki ? isareti aslinda na
apply(data,2,function(x){sum(x=="?")})

#gorsellestirme
library(ggplot2)
ggplot(data)+geom_bar(aes(x=income,fill=sex))
ggplot(data)+geom_bar(aes(x=income,fill=race))
#gelir 50Kdan buyuk olanlarin yuzde kaci beyaz
sum(data$race=="White"&data$income==">50K")/sum(data$income==">50K")
#gelir 50Kdan kucuk olanlarin yuzde kaci beyaz
sum(data$race=="White"&data$income=="<=50K")/sum(data$income=="<=50K")
#gelir 50Kdan buyuk olanlarin yuzde kaci siyahi
sum(data$race=="Black"&data$income==">50K")/sum(data$income==">50K")
#gelir 50Kdan buyuk olanlarin yuzde kaci diger
sum(data$race=="Other"&data$income==">50K")/sum(data$income==">50K")

#gelirin calisma saati boxplot
ggplot(data)+geom_boxplot(aes(x=income,y=hours.per.week))

#nalari duzeltme
levels(data$workclass)
data$workclass=as.character(data$workclass)
#eger ? var ise unkonow yap
data$workclass=ifelse(data$workclass=="?","Unknow",data$workclass)
str(data)
data$workclass=as.factor(data$workclass)
apply(data,2,function(x){sum(x=="?")}) 

data$occupation=as.character(data$occupation)
data$occupation=ifelse(data$occupation=="?","Unknow",data$occupation)
data$occupation=as.factor(data$occupation)

data$native.country=as.character(data$native.country)
data$native.country=ifelse(data$native.country=="?","Unknow",data$native.country)
data$native.country=as.factor(data$native.country)
apply(data,2,function(x){sum(x=="?")}) #nalari bitirdik

#occupation
ggplot(data)+geom_bar(aes(x=income,fill=occupation))
#unknow oranlari
sum(data$occupation=="Unknow"&data$income==">50K")/sum(data$income==">50K")
sum(data$occupation=="Unknow"&data$income=="<=50K")/sum(data$income=="<=50K")
#workclass
ggplot(data)+geom_bar(aes(x=income,fill=workclass))
sum(data$workclass=="Unknow"&data$income==">50K")/sum(data$income==">50K")
sum(data$workclass=="Unknow"&data$income=="<=50K")/sum(data$income=="<=50K")
#native country
ggplot(data)+geom_bar(aes(x=income,fill=native.country))
sum(data$native.country=="Unknow"&data$income==">50K")/sum(data$income==">50K")
sum(data$native.country=="Unknow"&data$income=="<=50K")/sum(data$income=="<=50K")

#####Naive Bayes###########################
#install.packages("e1071")
library(e1071)
#verinin 75 train 25 test olarak bolecegiz
set.seed(123)
rows=sample(1:length(data$age),round(0.75*length(data$age),digits = 0))
train=(data[rows,]) #row olanlar train
test=(data[-rows,]) #row olmayanlar test

#model kurma
model= naiveBayes(income~., data = train)
model

#tahmin yapma
pred=predict(model,test[,-15])
length(pred)

#modelin dogrulugunu bulma
table(test$income,pred)

#oran degisebilir
#toplam dogruluk
(5759+966)/(5759+426+989+966) 
#50> dogruluk
966/(966+989)
#50<= dogruluk
5759/(5759+426)
