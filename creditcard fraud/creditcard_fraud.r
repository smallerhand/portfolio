```{r}
#데이터 로드
creditcard<-read.csv("creditcard.csv", stringsAsFactors = F, header = T)
summary(creditcard)
creditcard<-creditcard[,-1]
sum(creditcard$Class)
sum(creditcard$Amount==0&creditcard$Class)
#사기인 492건 중 27건은 금액이 0임. 금액이 0인 경우는 위험하지 않다고 보아 분석에서 제외시킴.
#PCA처리 안된 Time이랑 Amount 중 Time은 제외시키고, Amount는 로그를 씌운 뒤 정규화.
creditcard<-creditcard[creditcard$Amount>0,]
hist(creditcard$Amount)
hist(log(creditcard$Amount))
logamount<-log(creditcard$Amount)
creditcard$Amount<-scale(logamount)
library(class)
nrow(creditcard)
#282982개 row
282982*.3
282982-84894
84894+198088
#84894개 sample로 test, 198088개 train
s<-sample(1:282982, 84894)
train<-creditcard[-s,]
test<-creditcard[s,]
```


```{r}
#knn모델 적용(k=11)
knnpre11<-knn(train[,-ncol(train)], test[,-ncol(test)], 
            train[,ncol(train)], k=11)
knnpre_v=c()
knnpre_v=list()
knnpre_v[1]=c(1,2,3,4,5)
class(knnpre11)
sum(knnpre11==test$Class)*100/nrow(test)
sum(test$Class==1&knnpre11==1)*100/sum(test$Class==1)
library(gmodels)
CrossTable(x=test[,ncol(test)], y=knnpre11,
           prop.chisq = FALSE)
#사기 128건 중 95건 사기로 분류, 33건 사기 아님으로 분류.
#사기 아님 84766건 중 18건 사기로 분류
```


```{r}
#C5.0모델 적용
library(C50)
train$Class<-as.factor(train$Class)
str(train)
c50model1<-C5.0(train[,-ncol(train)], train[,ncol(train)])
c50p1<-predict(c50model1,test[,-ncol(train)])
CrossTable(x=test[,ncol(test)], y=c50p1,
           prop.chisq = FALSE)
#사기 128건 중 83건 사기, 45건 사기아님으로 분류.
#사기 아님 84766건 중 15건 사기로 분류.
library(jpeg)
jpeg('creditcard_amount_log.jpeg')
hist(log(creditcard2$Amount))
dev.off()
```


```{r}
#C&RT모델 적용
library(rpart)
library(rpart.plot)
summary(train$Class)
str(train$Class)
rp1<-rpart(Class~.,data=train, method="anova")
rp2<-rpart(Class~.,data=train, method="class")
rpart.plot(rp2, type=3, digits = 3, fallen.leaves = TRUE)
rp1p<-predict(rp1, test[,-ncol(test)])
rp2p<-predict(rp2, test[,-ncol(test)])
summary(rp2p)
class(rp2p)
rp2pr<-NULL
for(i in 1:nrow(rp2p)){
  if(rp2p[i,1]<rp2p[i,2]){
    rp2pr[i]<-1
  }else{
    rp2pr[i]<-0
  }
}
rp2pr
sort(rp1p, decreasing = T)
rp1p[rp1p>0.5]<-1
rp1p[rp1p<=.5]<-0
CrossTable(x=test$Class, y=rp2pr,
           prop.chisq = FALSE)
#사기 128건 중 94건 사기, 34건 사기아님으로 분류.
#사기 아님 84766건 중 21건 사기로 분류.
#class로 했을 때나 anova로 했을때나 같음.
```


```{r}
#모델별 정확도 중간 확인
v<-c(18+33, 15+45, 34+21)
u<-v*100/84766
barplot(u)
u<-100-u
u2<-c(95,83,94)
u2<-u2*100/128
ud<-data.frame(Algorithm=c('kNN','C5.0','C&RT'),Accuracy=u,
               Accuracy_for_fraud=u2,stringsAsFactors = F)
ud$Algorithm<-ordered(ud$Algorithm, c('kNN','C5.0','C&RT'))
library(jpeg)
jpeg("Accuracy.jpeg", width = 600, height = 400)
ggplot(ud, aes(x=Algorithm, y=Accuracy))+
  geom_bar(stat='identity', width=.6, fill=rainbow_hcl(3))+
  geom_text(aes(label=paste0(round(ud$Accuracy, 4), '%'),), vjust=-.5, size=5)+
  coord_cartesian(ylim=c(99.900,99.960))+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19))
dev.off()
jpeg("Accuracy_for_fraud.jpeg", width = 600, height = 400)
ggplot(ud, aes(x=Algorithm, y=Accuracy_for_fraud))+
  geom_bar(stat='identity', width=.6, fill=heat_hcl(3))+
  geom_text(aes(label=paste0(round(ud$Accuracy_for_fraud, 4), '%'),), vjust=-.5, size=5)+
  coord_cartesian(ylim=c(50,80))+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19))
dev.off()
```


```{r}
#knn모델 성능을 높이기 위한 최적의 k 값 탐색
r<-NULL
knnpre_v1<-NULL
knnpre_v2<-NULL
for(i in 2:8){
  r<-knn(train[,-ncol(train)], test[,-ncol(test)], 
            train[,ncol(train)], k=i+2)
  Sys.sleep(.1)
  knnpre_v1[i]<-sum(r==test$Class)*100/nrow(test)
  knnpre_v2[i]<-sum(test$Class==1&r==1)*100/sum(test$Class==1)
  Sys.sleep(.1)
}


df<-data.frame(k=seq(3,11), 
               Accuracy=c(round(knnpre_v1,3), 99.940),
               Accuracy_for_fraud=c(round(knnpre_v2, 3), 74.219),
               stringsAsFactors = F)
library(ggplot2)
library(reshape2)
df2<-melt(df, id="k")
library(jpeg)
jpeg("Accuracy_by_k.jpeg", width = 1000, height = 400)
ggplot(df, aes(x=k))+
  geom_point(aes(y=Accuracy, colour="Accuracy"))+
  geom_line(aes(y=Accuracy, colour="Accuracy"))+
  geom_point(aes(y=Accuracy_for_fraud*1.3, colour="Accuracy_for_fraud"))+
  geom_line(aes(y=Accuracy_for_fraud*1.3, colour="Accuracy_for_fraud"))+
  geom_text(aes(label=paste0(round(Accuracy, 3), '%'),), vjust=-.5, size=5, colour="black")+
  scale_y_continuous(sec.axis = sec_axis(~./1.3, name = "for fraud"))
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19),
        legend.position = c(.8,.9))
dev.off()
```


```{r}
#k값에 따른 knn모델의 정확도 확인
jpeg("Accuracy_by_k.jpeg", width = 700, height = 400)
ggplot(df, aes(x=k, y=Accuracy))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=paste0(round(Accuracy, 3), '%')), vjust=-.5, size=4)+
  scale_x_continuous(breaks = seq(3,11,1))+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19))
dev.off()


jpeg("Accuracy_by_k2.jpeg", width = 700, height = 400)
ggplot(df, aes(x=k, y=Accuracy_for_fraud))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=paste0(round(Accuracy_for_fraud, 3), '%')), vjust=-.5, size=4)+
  scale_x_continuous(breaks = seq(3,11,1))+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19))
dev.off()
```


```{r}
#k=5로 knn모델 적용
knnpre5<-knn(train[,-ncol(train)], test[,-ncol(test)], 
            train[,ncol(train)], k=5)
library(gmodels)
CrossTable(x=test[,ncol(test)], y=knnpre5,
           prop.chisq = FALSE)
```