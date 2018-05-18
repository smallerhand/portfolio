#조선일보 정치/경제 기사 분류
#2-1버전. 100개씩 총 200개 기사 트레이닝 후 그 외 200개 기사로 테스트 90~91%정확도
#3번. 단어 횟수 1번 초과로, union(c3,c4)로 5030개 썼을 때 정확도 한겨레 조선 둘다 80% 정도.
#4번. c5<-union(c,c2)
#length(c5)
#10045개
#이 경우엔 조선, 한겨레 각각의 테스트 데이터에 대해 73, 85% 정확도. 한겨레에 대해서는 나름 높게 나오긴 했지만 두 경우 다 보면 위 경우랑 비슷함. 근데 돌아가는 시간은 훨씬 오래걸림.
컬럼(기준이 되는 단어)의 수가 너무 많으면 돌아가는 시간도 너무 오래걸릴 뿐만 아니라, 오버피팅 되어 테스트 데이터로 테스트했을 때 정확도가 낮아지는 것 같음. 모든 경우의 수에 대한 충분한 데이터가 없기 떄문.
#한겨레, 조선에서 나온 단어 합집합에서 2번이상 나타난 단어를 고르되, 분포도의 차이 상위권 단어들로 하면 좋지 않을까?
5번.names(ccount)<-c
length(ccount[ccount>20])
c5<-names(ccount[ccount>20])
head(c5)
length(c5)
#1089개
70, 84% 나옴. 컬럼 개수를 너무 줄였나? 그럼 이번엔 5천개정도까지만 늘려보자.
6번. length(ccount[ccount>4])
c5<-names(ccount[ccount>4])
head(c5)
length(c5)
#5059개
이렇게 해도 70, 82%. 오히려 떨어졌다..
3번이 차라리 낫네?? 왜일까.
위에서 차이를 그냥 뺀 걸로 했는데 1씩 더한 다음에 나눈 값으로 비교하는 게 낫지 않을까.
7번. ccount<-NULL
for(i in 1:length(c)){
  ccountcho<-1
  ccounthan<-1
  for(j in 1:1000){
    ccountcho<-ccountcho+ifelse(sum(chosun[[j]]==c[i])>0, 1, 0)
  }
  for(j in 1:1020){
    ccounthan<-ccounthan+ifelse(sum(han[[j]]==c[i])>0, 1, 0)
  }
  ccount[i]<-abs(log(ccountcho/ccounthan))
  Sys.sleep(.001)
}
paste(max(ccount), mean(ccount), min(ccount))
names(ccount)<-c
length(ccount[ccount>.91])
c5<-names(ccount[ccount>.91])
head(c5)
length(c5)
#5059개
79,87%
여기서 laplace를 0.1에서 1로 바꾸면 84, 82%
0.5로 하면 82,85%


8번. log를 log2로 바꾸고, 컬럼을 더 줄이자.
c는 총 15060개 단어(조선이랑 한겨레 전체에서 2번 이상 출현한 단어). 이 단어들 출현횟수를 카운트해서 한겨레, 조선 사이에 유의미한 차이가 있는 단어를 뽑음.
ccount<-NULL
for(i in 1:length(c)){
  ccountcho<-1
  ccounthan<-1
  for(j in 1:1000){
    ccountcho<-ccountcho+ifelse(sum(chosun[[j]]==c[i])>0, 1, 0)
  }
  for(j in 1:1020){
    ccounthan<-ccounthan+ifelse(sum(han[[j]]==c[i])>0, 1, 0)
  }
  ccount[i]<-abs(log2(ccountcho/ccounthan))
  Sys.sleep(.001)
}
paste(max(ccount), mean(ccount), min(ccount))
names(ccount)<-c
length(ccount[ccount>2])
c5<-names(ccount[ccount>2])
head(c5)
length(c5)
#1189개
laplace=.5일때, 84, 81%
1일때 85, 78%
.1일때, 81, 83%
9번. 단어를 조금만 늘려보자.
length(ccount[ccount>1.6])
c5<-names(ccount[ccount>1.6])
head(c5)
length(c5)
#2413개
laplace= .1, 77, 84%. 위 버전이 나음. 
#가장 많이 고민한 것: 통계적으로 유의미한 단어를 어떻게 거르느냐/laplace를 몇으로 하느냐.
10번.length(ccount[ccount>2.4])
c5<-names(ccount[ccount>2.4])
head(c5)
length(c5)
#667개
laplace=.5, 81,79%
11번.length(ccount[ccount>3])
c5<-names(ccount[ccount>3])
head(c5)
length(c5)
#224개
laplace=.5, 87, 69%
12번. c<-names(b3[b3>2])
length(c)
#11123개로 조절.
length(ccount[ccount>2])
c5<-names(ccount[ccount>2])
head(c5)
length(c5)
#1137개
laplace=.5, 84,81%




```{r}
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(KoNLP)
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="ojdbc6.jar")
library(rJava)
library(stringr)
library(rvest)
library(dplyr)
library(e1071)
#chosun 사설 1~100page(total 1000 case) 2017.1.9~현재날짜(2018.2.10)
text_cho<-NULL
for(i in 1:100){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
useNIADic()
t1<-extractNoun(text_cho)
chosun<-NULL
for(i in 1:1000){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  chosun[[i]]<-unique(a)
  Sys.sleep(.01)
}


#same to hangyeore
text_han<-NULL
#날짜를 맞추기 위해 한겨레는 68페이지까지 (2017.1.9~2018.2.10)
for(i in 1:68){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
  Sys.sleep(.1)
}


t_han1<-extractNoun(text_han)
han<-NULL
for(i in 1:1020){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  han[[i]]<-unique(a)
  Sys.sleep(.01)
}


b<-sort(table(unlist(chosun)), decreasing = T)
b2<-sort(table(unlist(han)), decreasing = T)
b3<-c(b,b2)
head(b3)
c<-names(b3[b3>1])
length(c)
head(c)
d<-NULL
for(i in 1:1000){
  d[i]<-length(intersect(chosun[[i]], c))
  Sys.sleep(.01)
}
paste(max(d), mean(d), min(d))
#341,104,20개
d2<-NULL
for(i in 1:1020){
  d2[i]<-length(intersect(han[[i]], c))
  Sys.sleep(.01)
}
paste(max(d2), mean(d2), min(d2))
#304, 110, 11개
ccount<-NULL
for(i in 1:length(c)){
  ccountcho<-1
  ccounthan<-1
  for(j in 1:1000){
    ccountcho<-ccountcho+ifelse(sum(chosun[[j]]==c[i])>0, 1, 0)
  }
  for(j in 1:1020){
    ccounthan<-ccounthan+ifelse(sum(han[[j]]==c[i])>0, 1, 0)
  }
  ccount[i]<-abs(log2(ccountcho/ccounthan))
  Sys.sleep(.001)
}
paste(max(ccount), mean(ccount), min(ccount))
names(ccount)<-c
length(ccount[ccount>3])
c5<-names(ccount[ccount>3])
head(c5)
length(c5)
#224개
m1<-matrix(rep(NA, length(c5)*1020), nrow=1020)
for(j in 1:length(c5)){
  for(i in 1:1020){
    m1[i, j]<-ifelse(sum(han[[i]]==c5[j])>0, 1, 0)
  }
  Sys.sleep(.001)
}
m2<-matrix(rep(NA, length(c5)*1000), nrow=1000)
for(j in 1:length(c5)){
  for(i in 1:1000){
    m2[i, j]<-ifelse(sum(chosun[[i]]==c5[j])>0, 1, 0)
  }
  Sys.sleep(.001)
}
dfhan <- as.data.frame(m1)
dfhan <- cbind(dfhan, label="h")
dfcho <- as.data.frame(m2)
dfcho <- cbind(dfcho, label="c")
df <- rbind(dfhan, dfcho)
for(i in 1:length(c5)){
  colnames(df)[i]<-c5[i]
  df[,i]<-as.logical(df[,i])
}
write.csv(df, "han_cho11.csv")


#기존 데이터로 다시 실행할 땐 여기부터
df<-read.csv("", header = T)
str(df)
ncol(df)
head(df)
df<-df[,-1]
c5<-colnames(df)[-ncol(df)]
#naiveBayes로 예측모델 생성
n1<-naiveBayes(df[,-ncol(df)], df[,ncol(df)], laplace = .5)
#우선 학습시킨 데이터로 검증해봄
sum(!predict(n1, df[1:1020, -ncol(df)])=="h")
#한겨레는 1020개 중에 91개 틀림
sum(!predict(n1, df[1021:nrow(df), -ncol(df)])=="c")
#조선일보는 1000개 중에 26개 틀림
#만약에 나이브베이즈가 없었다면 c3,c4각각이 얼마나 더 들어갔는지로 비교했을 것임. 그러면 결과가 어떻게 나오는지 테스트해보기. 또는 비율 차이로 뽑은 단어들로.


#이제 테스트 데이터 만듬.(2017.1.9 이전)
text_cho<-NULL
for(i in 1:30){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i+100))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
t1<-extractNoun(text_cho)
testchosun<-NULL
for(i in 1:300){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testchosun[[i]]<-unique(a)
}
c5<-colnames(df)[-ncol(df)]
m3<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m3[i, j]<-ifelse(sum(testchosun[[i]]==c5[j])>0, 1, 0)
  }
  Sys.sleep(.001)
}
dftestchosun <- as.data.frame(m3)
for(i in 1:length(c5)){
  colnames(dftestchosun)[i]<-c5[i]
  dftestchosun[,i]<-as.logical(dftestchosun[,i])
}
p1<-predict(n1, dftestchosun)
sum(p1=="c")/length(p1)*100
#85%정도 정확도.


text_han<-NULL
for(i in 1:30){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i+100, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
  Sys.sleep(.1)
}
t_han1<-extractNoun(text_han)
testhan<-NULL
for(i in 1:450){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testhan[[i]]<-unique(a)
}


m4<-matrix(rep(NA, length(c5)*450), nrow=450)
for(j in 1:length(c5)){
  for(i in 1:450){
    m4[i, j]<-ifelse(sum(testhan[[i]]==c5[j])>0, 1, 0)
  }
  Sys.sleep(.001)
}
dftesthan <- as.data.frame(m4)
for(i in 1:length(c5)){
  colnames(dftesthan)[i]<-c5[i]
  dftesthan[,i]<-as.logical(dftesthan[,i])
}
p2<-predict(n1, dftesthan)
sum(p2=="h")/length(p2)*100
#79%정확도.
```








```{r}
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(KoNLP)
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="desktop/r/ojdbc6.jar")
library(rJava)
library(stringr)
library(wordcloud2)
library(rvest)
library(dplyr)
library(e1071)
#chosun 사설 1~30page(total 300 case) 2017.10.16~현재날짜(2018.2.9)
text_cho<-NULL
for(i in 1:30){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
useNIADic()
t1<-extractNoun(text_cho)
chosun<-NULL
for(i in 1:300){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  chosun[[i]]<-unique(a)
}


b<-sort(table(unlist(chosun)), decreasing = T)
c<-names(b[b>10])
d<-NULL
for(i in 1:300){
  d[i]<-length(intersect(chosun[[i]], c))
}
max(d)
#115개
min(d)
#8개


#same to hangyeore
text_han<-NULL
#날짜를 맞추기 위해 한겨레는 20페이지까지 (2017.10.19~2018.2.9)
for(i in 1:20){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
}


t_han1<-extractNoun(text_han)
han<-NULL
for(i in 1:300){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  han[[i]]<-unique(a)
}


b2<-sort(table(unlist(han)), decreasing = T)
c2<-names(b2[b2>10])
d2<-NULL
for(i in 1:300){
  d2[i]<-length(intersect(han[[i]], c2))
}
max(d2)
#108개
min(d2)
#14개


length(c)
#671개
length(c2)
#728개
length(intersect(c,c2))
#490개
length(union(c,c2))
#909개


c3 <- setdiff(c,c2)
c4 <- setdiff(c2,c)
c5<-union(c3,c4)
length(c5)
#419개
m1<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m1[i, j]<-ifelse(sum(han[[i]]==c5[j])>0, 1, 0)
  }
}
m2<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m2[i, j]<-ifelse(sum(chosun[[i]]==c5[j])>0, 1, 0)
  }
}
dfhan <- as.data.frame(m1)
dfhan <- cbind(dfhan, label="h")
dfcho <- as.data.frame(m2)
dfcho <- cbind(dfcho, label="c")
df <- rbind(dfhan, dfcho)
for(i in 1:length(c5)){
  colnames(df)[i]<-c5[i]
  df[,i]<-as.logical(df[,i])
}
write.csv(df, "desktop/r/han_cho.csv")


#기존 데이터로 다시 실행할 땐 여기부터
df<-read.csv("desktop/r/han_cho.csv", header = T)
str(df)
head(df)
df<-df[,-1]
c5<-colnames(df)[-ncol(df)]
#어떤 단어들 들어갔는지 확인
ncol(df)
mcolname<-matrix(c(colnames(df), rep(NA, 80)),nrow=50, byrow=T)
View(mcolname)
#naiveBayes로 예측모델 생성
n1<-naiveBayes(df[,-ncol(df)], df[,ncol(df)], laplace = 1)
#우선 학습시킨 데이터로 검증해봄
nrow(df)
sum(!predict(n1, df[1:300, -ncol(df)])=="h")
#한겨레는 300개 중에 8개 틀림
sum(!predict(n1, df[301:nrow(df), -ncol(df)])=="c")
#조선일보는 300개 중에 7개 틀림


#이제 테스트 데이터 만듬.(2017.10.16 이전)
text_cho<-NULL
for(i in 1:30){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i+30))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
#메타데이터가 포함되진 않았는지 확인
head(text_cho)


t1<-extractNoun(text_cho)
testchosun<-NULL
for(i in 1:300){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testchosun[[i]]<-unique(a)
}


m3<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m3[i, j]<-ifelse(sum(testchosun[[i]]==c5[j])>0, 1, 0)
  }
}
dftestchosun <- as.data.frame(m3)
for(i in 1:300){
  colnames(dftestchosun)[i]<-c5[i]
  dftestchosun[,i]<-as.logical(dftestchosun[,i])
}
p1<-predict(n1, dftestchosun)
sum(p1=="c")/length(p1)*100
#85%정도 정확도


text_han<-NULL
for(i in 1:30){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i+20, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
  Sys.sleep(.1)
}
head(text_han)
t_han1<-extractNoun(text_han)
testhan<-NULL
for(i in 1:450){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testhan[[i]]<-unique(a)
}


m4<-matrix(rep(NA, length(c5)*450), nrow=450)
for(j in 1:length(c5)){
  for(i in 1:450){
    m4[i, j]<-ifelse(sum(testhan[[i]]==c5[j])>0, 1, 0)
  }
}
dftesthan <- as.data.frame(m4)
for(i in 1:length(c5)){
  colnames(dftesthan)[i]<-c5[i]
  dftesthan[,i]<-as.logical(dftesthan[,i])
}
p2<-predict(n1, dftesthan)
sum(p2=="h")/length(p2)*100
#86%정확도
hanorcho<-function(x){
  x<-readLines(paste0("desktop/r/",x,".txt"), encoding = "UTF-8")
  t<-unlist(extractNoun(x))
  a<-t[!str_detect(t, "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  test<-unique(a)
  m<-NULL
  for(j in 1:length(c5)){
    m[j]<-ifelse(sum(test==c5[j])>0, 1, 0)
  }
  dftest <- data.frame(matrix(m, nrow=1))
  for(i in 1:length(c5)){
    colnames(dftest)[i]<-c5[i]
    dftest[,i]<-as.logical(dftest[,i])
  }
  p<-predict(n1, dftest)
  return(p)
}


hanorcho("hantest")


```






```{r}
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(KoNLP)
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="desktop/r/ojdbc6.jar")
library(rJava)
library(stringr)
library(wordcloud2)
library(rvest)
library(dplyr)
library(e1071)
#chosun 사설 1~30page(total 300 case) 2017.10.16~현재날짜(2018.2.9)
text_cho<-NULL
for(i in 1:30){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
useNIADic()
t1<-extractNoun(text_cho)
chosun<-NULL
for(i in 1:300){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  chosun[[i]]<-unique(a)
}


b<-sort(table(unlist(chosun)), decreasing = T)
c<-names(b[b>10])
d<-NULL
for(i in 1:300){
  d[i]<-length(intersect(chosun[[i]], c))
}
max(d)
#115개
min(d)
#8개


#same to hangyeore
text_han<-NULL
#날짜를 맞추기 위해 한겨레는 20페이지까지 (2017.10.19~2018.2.9)
for(i in 1:20){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
}


t_han1<-extractNoun(text_han)
han<-NULL
for(i in 1:300){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  han[[i]]<-unique(a)
}


b2<-sort(table(unlist(han)), decreasing = T)
c2<-names(b2[b2>10])
d2<-NULL
for(i in 1:300){
  d2[i]<-length(intersect(han[[i]], c2))
}
max(d2)
#108개
min(d2)
#14개


length(c)
#671개
length(c2)
#728개
length(intersect(c,c2))
#490개
length(union(c,c2))
#909개


c3 <- setdiff(c,c2)
c4 <- setdiff(c2,c)
c5<-union(c3,c4)
length(c5)
#419개
m1<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m1[i, j]<-ifelse(sum(han[[i]]==c5[j])>0, 1, 0)
  }
}
m2<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m2[i, j]<-ifelse(sum(chosun[[i]]==c5[j])>0, 1, 0)
  }
}
dfhan <- as.data.frame(m1)
dfhan <- cbind(dfhan, label="h")
dfcho <- as.data.frame(m2)
dfcho <- cbind(dfcho, label="c")
df <- rbind(dfhan, dfcho)
for(i in 1:length(c5)){
  colnames(df)[i]<-c5[i]
  df[,i]<-as.logical(df[,i])
}
write.csv(df, "desktop/r/han_cho.csv")
n1<-naiveBayes(df[,-ncol(df)], df[,ncol(df)], laplace = 1)
#우선 학습시킨 데이터로 검증해봄
nrow(df)
sum(!predict(n1, df[1:300, -ncol(df)])=="h")
#한겨레는 300개 중에 8개 틀림
sum(!predict(n1, df[301:nrow(df), -ncol(df)])=="c")
#조선일보는 300개 중에 7개 틀림


#이제 테스트 데이터 만듬.(2017.10.16 이전)
text_cho<-NULL
for(i in 1:30){
  html<- read_html(paste0("http://news.chosun.com/svc/list_in/list.html?catid=617&pn=", i+30))
  url <- html_nodes(html,css="dl.list_item dd.desc") %>%
    html_nodes('a') %>%
    html_attr("href")
  a<-length(text_cho)
  for(j in 1:length(url)){
    html2<-read_html(url[j])
    text_cho[a+j]<-html_node(html2, css=".news_body .par") %>%
      html_text()
  }
  Sys.sleep(.1)
}
t1<-extractNoun(text_cho)
testchosun<-NULL
for(i in 1:300){
  a<-t1[[i]][!str_detect(t1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testchosun[[i]]<-unique(a)
}


m3<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m3[i, j]<-ifelse(sum(testchosun[[i]]==c5[j])>0, 1, 0)
  }
}
dftestchosun <- as.data.frame(m3)
for(i in 1:300){
  colnames(dftestchosun)[i]<-c5[i]
  dftestchosun[,i]<-as.logical(dftestchosun[,i])
}
p1<-predict(n1, dftestchosun)
sum(p1=="c")/length(p1)*100
#84%정도 정확도


text_han<-NULL
for(i in 1:30){
  html_han<-read_html(paste0("http://www.hani.co.kr/arti/opinion/editorial/list", i+20, ".html"))
  url_han<-html_nodes(html_han, css=".section-list-area .list .article-title")%>%
    html_nodes("a")%>%
    html_attr("href")
  url_han<-paste0("http://www.hani.co.kr", url_han)
  a<-length(text_han)
  for(j in 1:length(url_han)){
    html2<-read_html(url_han[j])
    text_han[a+j]<-html_nodes(html2, css=".article-text .text")%>%
      html_text()
  }
  Sys.sleep(.1)
}


t_han1<-extractNoun(text_han)
testhan<-NULL
for(i in 1:300){
  a<-t_han1[[i]][!str_detect(t_han1[[i]], "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  testhan[[i]]<-unique(a)
}


m4<-matrix(rep(NA, length(c5)*300), nrow=300)
for(j in 1:length(c5)){
  for(i in 1:300){
    m4[i, j]<-ifelse(sum(testhan[[i]]==c5[j])>0, 1, 0)
  }
}
dftesthan <- as.data.frame(m4)
p2<-predict(n1, dftesthan)
sum(p2=="h")/length(p2)*100
#100%정확도. 맞는건가?? ;;
#맞는 듯... 너무 정확한데? 이제 함수 만들어보자
hanorcho<-function(x){
  x<-readLines(paste0("desktop/r/",x,".txt"), encoding = "UTF-8")
  t<-unlist(extractNoun(x))
  a<-t[!str_detect(t, "[^[가-힣]]")]
  a<-a[nchar(a)>1]
  test<-unique(a)
  m<-NULL
  for(j in 1:length(c5)){
    m[j]<-ifelse(sum(test==c5[j])>0, 1, 0)
  }
  dftest <- data.frame(matrix(m, nrow=1))
  for(i in 1:419){
    colnames(dftest)[i]<-c5[i]
    dftest[,i]<-as.logical(dftest[,i])
  }
  p<-predict(n1, dftest)
  return(p)
}


hanorcho("hantest")
```














```{r}
#우선 최대공약수를 출력하는 함수를 만듬.
#최대공약수 출력하는 함수 f2를 만들기 위해 소인수분해하는 함수 f1을 만들어서 f2안에 넣음. f2는 f1의 출력값 행렬을 가지고 최대공약수를 계산함.
f2<-function(A,B){
  f1<-function(a){
    b<-NULL; c<-NULL; j<-1;
    for(i in 2:trunc(sqrt(a))){
      if(a%%i==0){
        a<-a/i; b[j]<-i; c[j]<-1;
        repeat{
          if(a%%i!=0){
            break }
          a<-a/i; c[j]<-c[j]+1 } 
        j<-j+1 }
      i<-i+1 }
    if(a!=1){
      b[j]<-a; c[j]<-1 } 
    return(matrix(c(b,c), nrow=2, byrow=T)) }
  m1<-f1(A); m2<-f1(B); d<-1;
  for(j in 1:ncol(m2)){
    for(i in 1:ncol(m1)){
      if(m1[1,i]==m2[1,j]){
        d<-d*(m1[1,i]**min(m1[2,i],m2[2,j]))
        } } }
  return(d) }


prod(2**3, 3**4, 5, 7**2, 13)
prod(2**2, 3**2, 7, 13**3)
#최대공약수 잘 나오는 거 검증
prod(2**2, 3**2, 7, 13)==f2(2063880,553644)
#이제 최대공약수 D는 f2(A,B)로 구할 수 있음. 
#아래에서 X, Y를 구함.
#최대공약수 D는 A,B보다 작거나 같으므로 AX+BY=D를 만족하는 정수 X,Y는 서로 부호가 반대이거나 하나가 0이어야 함. 근데 하나가 0인 경우엔 다른 하나는 양수여야 함. A<B 이면 Y가 음수일 때 X는 Y보다 절대값이 큰 양수여야 함.
#X,Y절대값 합이 1,2,3,4,...인 경우 순차로 구함
Y<-1; X<-0
Y<-0; X<-1


Y<-2; X<-0
Y<-1; X<--1
Y<-0; X<-2


Y<-3; X<-0
Y<-2; X<--1
Y<-1; X<-2 | X<--2
Y<-0; X<-3
Y<--1; X<-2


AX+BY==f2(A,B)




```








2나 5로 나눌 수 없는 0 이상 10,000 이하의 정수 n이 주어졌는데, n의 배수 중에는 10진수로 표기했을 때 모든 자리 숫자가 1인 것이 있다. 그러한 n의 배수 중에서 가장 작은 것은 몇 자리 수일까?
Sample Input
3
7
9901
Sample Output
3
6
12
```{r}
n<-9901
i<-2
if(n%%2!=0&n%%5!=0){
  repeat{
    a<-n*i
    c<-NULL
    b<-trunc(log10(a))
    for(j in 0:b){
      c[j+1]<-(a%%(10**(j+1)))%/%(10**j)==1
    }
    if(prod(c)==1){
      print(length(c))
      break
    }
    i<-i+1
  }
}


```






f2<-function(...){
f1<-function(s, number){
number<-unlist(strsplit(as.character(number), ""))
a<-length(number)
for(i in 1:a){
 cat(" ")
 for(j in 1:s){
 if(number[i]%in%c("1","4")){
   cat(" ")
 }else if(number[i]%in%c("2","3","5","6","7","8","9","0")){
   cat("-") }}
 cat(" ")
 cat(" ") }
cat("\n")
for(j in 1:s){
for(i in 1:a){
 if(number[i]=="1"){
   cat(" ")
   for(k in 1:s){
     cat(" ") }
   cat("|")
 }else if(number[i]%in%c("2","3","7")){
   cat(" ")
   for(k in 1:s){
     cat(" ") }
   cat("|")
 }else if(number[i]%in%c("4","8","9","0")){
   cat("|")
   for(k in 1:s){
     cat(" ") }
   cat("|")
 }else if(number[i]%in%c("5","6")){
   cat("|")
   for(k in 1:s){
     cat(" ") }
   cat(" ") }
 cat(" ") }
cat("\n") }
for(i in 1:a){
 cat(" ")
 for(j in 1:s){
 if(number[i]%in%c("1","7","0")){
   cat(" ")
 }else if(number[i]%in%c("2","3","4","5","6","8","9")){
   cat("-") }}
 cat(" ")
 cat(" ") }
cat("\n")
for(j in 1:s){
for(i in 1:a){
 if(number[i]=="1"){
   cat(" ")
   for(k in 1:s){
     cat(" ") }
   cat("|")
 }else if(number[i]%in%c("3","4","5","7","9")){
   cat(" ")
   for(k in 1:s){
     cat(" ") }    
   cat("|")
 }else if(number[i]%in%c("6","8","0")){
   cat("|")
   for(k in 1:s){
     cat(" ") }    
   cat("|")
 }else if(number[i]%in%c("2")){
   cat("|")
   for(k in 1:s){
     cat(" ") }
   cat(" ") }
 cat(" ") }
cat("\n") }
for(i in 1:a){
 cat(" ")
 for(j in 1:s){
 if(number[i]%in%c("1","4","7")){
   cat(" ")
 }else if(number[i]%in%c("2","3","5","6","8","9","0")){
   cat("-") }}
 cat(" ")
 cat(" ") }
cat("\n") } 
 v<-c(...)
 if(v[length(v)]==0&v[length(v)-1]==0){
   for(l in 1:((length(v)/2)-1)){
     f1(v[2*l-1],v[2*l])
   }
 }
}

f2(2,12345,3,67890,0,0)












  0   1   2   3   4   5
 19  20  21  22  23   6
 18  31  32  33  24   7
 17  30  35  34  25   8
 16  29  28  27  26   9
 15  14  13  12  11  10
 직사각형인 경우까지 고려해서 x랑 y를 입력변수로.
 
```{r}
x<-7
y<-6
m<-matrix(rep(NA, x*y), nrow=y, ncol=x)
i<-1
j<-0
k<-0
a<-1
m
repeat{
  if(sum(is.na(m))==0){
    break
  }
        if(k==22){
        break
      }
  if(a%%2==1){
    j<-j+(-1)**(((a-1)%/%2)%%2)
    repeat{
      Sys.sleep(.1)
      if(k==22){
        break
      }
      if(j>x){
        a<-a+1
        j<-j-1
        break
        }
      if(j==0){
        a<-a+1
        j<-1
        break
        }
      if(!is.na(m[i,j])){
        a<-a+1
        break
        }
      m[i,j]<-k
      j<-j+(-1)**(((a-1)%/%2)%%2)
      k<-k+1
      }}else{
        i<-i+(-1)**(((a-1)%/%2)%%2)
        repeat{
          Sys.sleep(.1)
                if(k==22){
        break
      }
          if(i>y){
            a<-a+1
            i<-i-1
            break
            }
          if(i==0){
            a<-a+1
            i<-1
            break
            }
          if(!is.na(m[i,j])){
            a<-a+1
            break
            }
          m[i,j]<-k
          i<-i+(-1)**(((a-1)%/%2)%%2)
          k<-k+1
        } }
  }
m
```
i 3
j 2
a 6
k 23


i 1
j 1
a 4
k 22










1 부터 9까지의 연속된 수를 + 나 - 를 사용하여 합계가 100이 되는 전체 수를 구하시오.
ex) 1 + 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100
```{r}
library(stringr)
s<-"123456789"
s<-as.numeric(unlist(str_split(s, "")))
s
#2개~10개로 숫자 나눌 수 있음. 10개 숫자의 사이 9군데 중 2개~9개를 골라서 나누므로 10C2+10C3+...10C9개 경우의 수.


ncol(combn(10, 3))
10*9*8/2/3
choose(10,2)
choose(10,3)
r<-0
for(i in 2:9){
  r<-r+choose(10, i)
}


r
a<-combn(10,2)
ncol(a)
a[,1] #1, 2 
s     #1  2  3456...89
b<-NULL
for(i in 1:ncol(a)){
  b[i]<-a[,i]
}


```


```{r}
i<-1
j<-1
m<-matrix(rep(NA, 36), nrow=6)
k<-0
l<-1 
while(sum(is.na(m))>0){
  if(i>0&i<=6&j>0&j<=6){
    if(is.na(m[i,j])){
    m[i,j]<-k
    j<-j+l
    k<-k+1
    }
    }else{
    m<-t(m)
    a<-i
    i<-j 
    j<-a
    }
}
m
```
  0   1   2   3   4   5
 19  20  21  22  23   6
 18  31  32  33  24   7
 17  30  35  34  25   8
 16  29  28  27  26   9
 15  14  13  12  11  10
 직사각형인 경우까지 고려해서 x랑 y를 입력변수로.
 
```{r}
x<-3
y<-4
m<-matrix(rep(NA, x*y), nrow=y, ncol=x)
i<-1
j<-1
k<-0
a<-1
#아래처럼 빙빙돌게는 만들었는데 값이 na가 아니면 방향 바꾸는 거 추가해야 함.
while(sum(is.na(m))>0){
if(a%%4==1){
while(i>0&i<=y&j>0&j<=x){
  if(is.na(m[i,j])){
  m[i,j]<-k
  j<-j+1
  k<-k+1 
  }
} }else{
  if(a%%4==2){
    while(i>0&i<=y&j>0&j<=x){
      if(is.na(m[i,j])){
      m[i,j]<-k
      i<-i+1
      k<-k+1 
      }
    } }else{
      if(a%%4==3){
        while(i>0&i<=y&j>0&j<=x){
          if(is.na(m[i,j])){
           m[i,j]<-k
          j<-j-1
          k<-k+1 
          }
        } }else{
            while(i>0&i<=y&j>0&j<=x){
              if(is.na(m[i,j])){
               m[i,j]<-k
              i<-i-1
              k<-k+1 
              }
              } } } }
a<-a+1
if(sum(!is.na(m[i,j]))==k){
  k<-k-1
}
if(i==y+1){
    i<-i-1
}else{
  if(j==x+1){
    j<-j-1
  }else{
  if(i==0){
    i<-i+1
  }else{
  if(j==0){
    j<-j+1
} } } }
}
m
```






문제는 다음과 같다:
6 6
  0   1   2   3   4   5
 19  20  21  22  23   6
 18  31  32  33  24   7
 17  30  35  34  25   8
 16  29  28  27  26   9
 15  14  13  12  11  10
위처럼 6 6이라는 입력을 주면 6 X 6 매트릭스에 나선형 회전을 한 값을 출력해야 한다. 
```{r}
i<-1
j<-1
m<-matrix(rep(NA, 36), nrow=6)
k<-0
for(a in 1:2){
while(j<=6){
  m[i,j]<-k
  j<-j+1
  k<-k+1
}
j<-j-1
i<-i+1
t<-i
i<-j
j<-t
m<-t(m)


while(j>=2){
  m[i,j]<-k
  j<-j-1
  k<-k+1
}
}
t(m)
m
paste(i,j,k)
```




문제는 다음과 같다:
6 6
  0   1   2   3   4   5
 19  20  21  22  23   6
 18  31  32  33  24   7
 17  30  35  34  25   8
 16  29  28  27  26   9
 15  14  13  12  11  10
위처럼 6 6이라는 입력을 주면 6 X 6 매트릭스에 나선형 회전을 한 값을 출력해야 한다.
123456 66666 54321 1111 2345 555 432 22 34 4 3
111111 23456 66666 5432 2222 345 555 43 33 4 4
1 6 6 1 2 5 5 2 3 4 4 
6 5 5 4 4 3 3 2 2 1 1
7개면 1 7 7 1 2 6 6 2 3 5 5 3 4
      7 6 6 5 5 4 4 3 3 2 2 1 1 
입력이 x 일 때, 각 파트별 개수는 n이라고 함. 
```{r}
cnt<-function(x){
  n<-NULL
  i<-1
  n[1]<-x
  while(sum(n)<x**2){
    i<-i+1
    n[i]<-x-i%/%2
  }
  return(n)
}


cnt(6)
library(stringr)
v<-NULL
u<-NULL
for(i in 1:cnt(6)[1]){
  v[i]<-i
}
v
a<-length(v)
for(i in 1:(cnt(6)[2])){
  v[a+i]<-6
}
v
a<-length(v)
for(i in 1:(cnt(6)[3])){
  v[a+i]<-6-i
}
v
a<-length(v)
for(i in 1:(cnt(6)[4])){
  v[a+i]<-1
}
v
a<-length(v)
for(i in 1:(cnt(6)[4])){
  v[a+i]<-1+i
}
v


mat<-function(y){
  m<-matrix(rep(NA, y**2), nrow=y)
  for(i in 0:(y**2)){
    m[u[i+1],v[i+1]]<-i
  }
  return(m)
}
```




There was little or no conversation or questioning around cleansing, prep, transforms, feature engineering, feature selection, model selection, and absolutely none about about hyperparameter tuning.


```{r}
#우선 n자리수에 앞뒤 같은 수가 몇개인지 계산하는 함수를 만듬.
  a <- function(n){
    if(n==1){
      return(10)     #1자리수는 10개임.
    } else {
      if(n==2){
        return(9)    #2자리수(11~99)는 9개임.
        } else {
          m <- n%/%2
          if(n%%2==1){    #3,5,7,9,...자리수인 경우
            return((10**m-10**(m-1)) * a(1))
            } else {      #4,6,8,10,...자리수인 경우
              return(10**m-10**(m-1)) 
            }}}}
#근데 n이 617~619일 때 Inf로 나오고, 620이상이면 NaN이 나옴. 
#R의 오류인지 이 함수의 오류인지 확인 못 함. 하지만 617자릿수 미만까진 문제 없을 것으로 봄.
#예시: a(1)+a(2)+a(3) = 109이므로 0~999중 앞뒤 같은 수는 109개
a(1)
a(1)+a(2)+a(3)
"그럼 110번째 앞뒤같은 수는 1001임.
111번째 앞뒤 같은 수는 1111이고, 112번째는 1221.
m번째 앞뒤 같은 수가 짝수자리수라는 걸 알고 있다면, 
자리수 k의 절반 2/k자리수를 10..00부터 시작하여 몇번째 수인지 찾음. 즉 m-(a(1)+a(2)+a(3)...)+9..99임"
"m번째 앞뒤 같은 수가 홀수자리수라면,
(k-1)/2 자리수가 10..00부터 99..99까지 x개라 할 때,
x%/%(m-(a(1)+a(2)+a(3)...))이 가운데 숫자가 되고,
x%%(m-(a(1)+a(2)+a(3)...))가 앞숫자가 됨."
#m번째 앞뒤 같은 수를 구함
m<-110
k1<-a(1)+a(2)+a(3)
m-k>0
k2<-k1+a(4)
m-k2>0
#k1<m<k2 이므로 m번째 앞뒤 같은 수는 4자리 수임.
r<-m-k1+9
#앞의 절반은 10임
result<-paste0(r,r%%10,r%/%10%%10)
result


apdui<-function(m){
  if(m<=10){
    return(m-1)
  }else{
k<-a(1)
i<-2
while(m>k){
  k<-k+a(i)
  i<-i+1
}
#m번째 앞뒤 같은 수는 i-1자리수임. 여기선 짝수자리수인 경우.
#앞 절반은 (i-1)/2 자리수이므로 (i-1)/2-1자리수 중 제일 큰수에 m-k+a(i-1)을 더하면 됨.
if(i%%2==1){
r<-10**((i-1)/2-1)-1+m-k+a(i-1)
result<-r
c<-trunc(log10(r))
for(h in 1:(c+1)){
  result<-paste0(result,r%%10)
  r<-r%/%10
}
return(result)
}else{
#i-1이 홀수(m번째 앞뒤 같은 수가 홀수자리수)가 되는 경우, 가운데 숫자는 0~9까지 10개가 올 수 있으므로 (m-k+a(i-1))%/%10+1이 앞숫자가 되고, ((m-k+a(i-1)))%%10-1이 가운데숫자가 됨.
r1<-(m-k+a(i-1))%/%10+10**((i-2)/2-1)
r2<-((m-k+a(i-1)))%%10-1
result<-paste0(r1, r2)
c<-trunc(log10(r1))
for(h in 1:(c+1)){
  result<-paste0(result,r1%%10)
  r1<-r1%/%10
}
return(result)
}}}


apdui(1000000)
```










```{r}
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(KoNLP)
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="desktop/r/ojdbc6.jar")
library(rJava)
useSejongDic()
useNIADic()
library(stringr)
library(rvest)
library(dplyr)
t<-NULL
party<-c("국민의당", "더불어민주당",
         "바른정당", "자유한국당", "정의당")
addr<-c("http://watch.peoplepower21.org/?mid=AssemblyMembers&mode=search&party=%EA%B5%AD%EB%AF%BC%EC%9D%98%EB%8B%B9&region=&sangim=&gender=&elect_num=&page=", 
        "http://watch.peoplepower21.org/?mid=AssemblyMembers&mode=search&party=%EB%8D%94%EB%B6%88%EC%96%B4%EB%AF%BC%EC%A3%BC%EB%8B%B9&region=&sangim=&gender=&elect_num=&page=",
        "http://watch.peoplepower21.org/?act=&mid=AssemblyMembers&vid=&mode=search&name=&party=%EB%B0%94%EB%A5%B8%EC%A0%95%EB%8B%B9&region=&sangim=&gender=&age=&elect_num=&page=",
        "http://watch.peoplepower21.org/?mid=AssemblyMembers&mode=search&party=%EC%9E%90%EC%9C%A0%ED%95%9C%EA%B5%AD%EB%8B%B9&region=&sangim=&gender=&elect_num=&page=",
        "http://watch.peoplepower21.org/?act=&mid=AssemblyMembers&vid=&mode=search&name=&party=%EC%A0%95%EC%9D%98%EB%8B%B9&region=&sangim=&gender=&age=&elect_num=&page=")
pt<-data.frame(party=party, html=addr)


for(a in 1:5){
  b<-1
  c<-1
while(c!=0){
  h<- read_html(paste0(pt$html[a], b))
  url <- html_nodes(h, css="a, a:hover") %>%
  html_attr('href')
  url<-url[str_detect(url, "Member&member_seq")]
  url<-paste0("http://watch.peoplepower21.org", url)
  link<-NULL
for(i in 1:length(url)){
  if(i%%2==0){
    link[i/2]<-url[i]
  }
}
  b<-b+1
  c<-length(link)
  if(c!=0){
txt0 <- html_nodes(h,css="a") %>%
  html_text()
txt0<-txt0[str_detect(txt0, "●")]
name<-unlist(str_extract_all(txt0, "[가-힣]{2,4}"))
money<- NULL
for(i in 1:length(link)){
h2<- read_html(link[i])
txt2<-html_nodes(h2, ".info")%>%
  html_text()
if(length(txt2)>0){
if(str_detect(txt2, ".*,[0-9]{3}[0-9]+.*[0-9]+.*")){
  money[i]<-sub(".*,[0-9]{3}([0-9]+.*[0-9]+).*$", "\\1", txt2)
}else{
  money[i]<-NA
}
}
}
t<-rbind(t, data.frame(party=pt$party[a], name=name, money=money, stringsAsFactors = F))
}
}
}


t$money<-gsub(",","",t$money)
t$money <- as.numeric(t$money)
avg<-aggregate(money~party, t, mean, na.rm=T)
avg
library(ggplot2)
ggplot(avg, aes(x=party, y=sqrt(money), fill=party))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(round(money/1000),'백만원')), vjust=-.5, size=3)


```












쏠리드 주식토론 게시판 분석
```{r}
library(KoNLP)  
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
library(stringr)
library(wordcloud2)
library(rvest)
library(dplyr)


html<- read_html("http://finance.naver.com/item/board.nhn?code=050890", encoding = "euc-kr")
url <- html_nodes(html,css="table.type2 td.title") %>%
  html_nodes('a')%>%
  html_attr('href')
url<-paste0("http://finance.naver.com", url)
txt<-NULL
for(i in 1:length(url)){
html1<- read_html(url[i], encoding = "euc-kr")
txt[i]<-html_node(html1, "#body")%>%
  html_text()
}


for(i in 2:20){
html<- read_html(paste0("http://finance.naver.com/item/board.nhn?code=050890&page=", i), encoding = "euc-kr")
url <- html_nodes(html,css="table.type2 td.title") %>%
  html_nodes('a')%>%
  html_attr('href')
url<-paste0("http://finance.naver.com", url)
a<-length(txt)
for(j in 1:length(url)){
html1<- read_html(url[j], encoding = "euc-kr")
txt[a+j]<-html_node(html1, "#body")%>%
  html_text()
}
}


txt<-gsub('\r',' ',txt)
t1<-SimplePos09(txt)
t2<-unlist(str_match_all(t1, '([A-Z가-힣]+)/N'))
t2<-t2[!str_detect(t2, '/')]
head(t2)


t3<-t2[str_length(t2)>1]
t6<-table(t3)


t6<-t6[t6>3]
t7<-sort(t6, decreasing = T)
head(t7, 100)
head(names(t7),100)                                   
wordcloud2(t6, size=.5)
```






한겨레랑 조선일보 사설 분석
```{r}
library(KoNLP)  
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
library(stringr)
library(wordcloud2)
han <- readLines("c:/r/han20021.txt") 
jo <- readLines("c:/r/jo20021.txt") 


v<-NULL
j<-1
for(i in 1:length(han)){
  loc<-str_locate_all(han[i],'<p>.*</p>')
  if(length(loc[[1]])>0){
    v[j]<-substr(han[i], loc[[1]][1,1]+3, loc[[1]][1,2]-4)
    j<-j+1
  }
}


head(v)


u<-NULL
j<-1
for(i in 1:length(jo)){
  loc<-str_locate_all(jo[i],'<p>.*</p>')
  if(length(loc[[1]])>0){
    u[j]<-substr(jo[i], loc[[1]][1,1]+3, loc[[1]][1,2]-4)
    j<-j+1
  }
}


head(u)


v2<-SimplePos09(v)
v3<-unlist(str_match_all(v2, '([A-Z가-힣]+)/N'))
v3<-v3[!str_detect(v3, '/')]
head(v3)
del <- read.table("c:/r/del.txt")
del1<-as.vector(as.matrix(del))
for(i in 1:length(del1)){
  t3[t3==del1[i]]<-''
}
v4<-v3[str_length(v3)>1]
v6<-table(v4)
v6<-v6[v6>6]
v7<-sort(v6, decreasing = T)
head(v7, 100)
head(names(v7),100)                                   
#일단 결과물 확인한 후 단어들 텍스트로 저장
data<-names(v7)
write(unlist(data),"c:/r/vdel.txt")                      
#텍스트로 저장된 데이터 중에 필요한 단어는 제외한 뒤 위에서 제거
wordcloud2(v6, size=.5)


u2<-SimplePos09(u)
u3<-unlist(str_match_all(u2, '([A-Z가-힣]+)/N'))
u3<-v3[!str_detect(u3, '/')]
head(u3)
del <- read.table("c:/r/del.txt")
del1<-as.vector(as.matrix(del))
for(i in 1:length(del1)){
  t3[t3==del1[i]]<-''
}
u4<-u3[str_length(u3)>1]
u6<-table(u4)
u6<-u6[u6>6]
u7<-sort(u6, decreasing = T)
head(u7, 100)
head(names(u7),100)                                   
#일단 결과물 확인한 후 단어들 텍스트로 저장
data<-names(u7)
write(unlist(data),"c:/r/vdel.txt")                      
#텍스트로 저장된 데이터 중에 필요한 단어는 제외한 뒤 위에서 제거
wordcloud2(u6, size=.5)


setdiff(head(names(u7),100), head(names(v7),100))
setdiff(head(names(v7),100), head(names(u7),100))


```






________________




국립중앙박물관
홍대
선유도공원
하늘공원
북악
삼청동
민속박물관
벽화마을
북촌한옥마을
인사동
광장시장
덕수궁
명동
남산
남산타워
전쟁기념관


(1) 빈도수 높은 단어들 보여줌
```{r}
library(KoNLP)  
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
library(stringr)
library(wordcloud2)
text <- readLines("c:/r/seoul_trip.txt") 
t2<-SimplePos09(text)
t3<-unlist(str_match_all(t2, '([A-Z가-힣]+)/N'))
t3<-t3[!str_detect(t3, '/')]
t3[str_detect(t3, '타워')]<-'남산타워'
t3[str_detect(t3, '덕수궁')]<-'덕수궁'
t3[str_detect(t3, '북촌한옥')]<-'북촌한옥마을'
t3[str_detect(t3, '남산한옥')]<-'남산한옥마을'
t3[str_detect(t3, '홍대')]<-'홍대'
t3[str_detect(t3, '북악')]<-'북악'
t3[str_detect(t3, "선유")]<-"선유도공원"
t3[str_detect(t3, "광장")]<-"광장시장"
t3[str_detect(t3, '벽화마을')]<-'벽화마을'
t3[str_detect(t3, '몽촌')]<-'몽촌토성'
t3[str_detect(t3, '뚝섬')]<-'뚝섬유원지'
t3[str_detect(t3, '남대문')]<-'남대문'
t3[str_detect(t3, '세운초록띠')]<-'세운초록띠공원'
t3[str_detect(t3, '광장')]<-'광장시장'
t3[str_detect(t3, '전쟁기념')]<-'전쟁기념관'
t3[str_detect(t3, '철도길')|str_detect(t3, '기찻길')]<-'기찻길'
t3[str_detect(t3, '명동')]<-'명동'
t3[str_detect(t3, "관광")|
     str_detect(t3, '명소')]<-''


del <- read.table("c:/r/del.txt")
del1<-as.vector(as.matrix(del))
for(i in 1:length(del1)){
  t3[t3==del1[i]]<-''
}
t4<-t3[str_length(t3)>1]
t6<-table(t4)
t6<-t6[t6>2]
t7<-sort(t6, decreasing = T)
head(t7, 100)
head(names(t7),100)                                   
#일단 결과물 확인한 후 단어들 텍스트로 저장
data<-names(t7)
write(unlist(data),"c:/r/del.txt")                      
#텍스트로 저장된 데이터 중에 필요한 단어는 제외한 뒤 위에서 제거
wordcloud2(t6, size=.5)
#단어시각화는 위처럼. 아래는 지도에 출력하는 과정.
t7<-sort(t6, decreasing = T)
t7
library(colorspace)
barplot(t7, border=NA, col=heat_hcl(length(t7)), las=3, cex.axis = .8, cex.names = .8)
t7






```


```{r}
head(text)


table(unlist(str_extract_all(text, 
'[[:alpha:]]{0, }[[:space:]]{0, }한옥[[:space:]]{0,}[[:alpha:]]{0,}')))


gsub('a', '=','aabbcc')


```




________________






문제는 다음과 같다:


6 6


  0   1   2   3   4   5
 19  20  21  22  23   6
 18  31  32  33  24   7
 17  30  35  34  25   8
 16  29  28  27  26   9
 15  14  13  12  11  10
위처럼 6 6이라는 입력을 주면 6 X 6 매트릭스에 나선형 회전을 한 값을 출력해야 한다.


```{r}
n<-matrix(NA, nrow=6, ncol=6)
x<-6
y<-6
k<-0
i<-1
j<-1


while(k < (x*y)){
  while(j < y){
    n[i,j]<-k
    k<-k+1
    j<-j+1
  }
  x<-x-1
  y<-y-1
  while(i <= x){
    n[i,j]<-k
    k<-k+1
    i<-i+1
  }
  while(j > 0){
    n[i,j]<-k
    k<-k+1
    j<-j-1
  }
  x<-x-1
  y<-y-1
  j<-j+1
  while(i > 1){
    n[i,j]<-k
    k<-k+1
    i<-i-1
  }
}




n
```




```{r}
library("ggmap")        
library("geosphere")
library(ggplot2)
names<-c("협재해수욕장","하귀","풍차","천지연폭포","천제연폭포","중문관광단지","주상절리","전망대","우도","용머리해안","외돌개","오설록","여미지식물원","에코랜드","애월해안도로","수월봉","쇠소깍","송악산","성산일출봉","섭지코지","산방산","산굼부리","박물관","만장굴","러브랜드","대장금","녹차")
names<-paste('제주', names)
gc<-geocode(enc2utf8(names))
gc
df<-data.frame(name = names, lon=gc$lon, lat=gc$lat)
names2<-as.vector(df[is.na(df$lon),1])
df[df$name==names2,c(2:3)]<-geocode(enc2utf8(names2))
df


cen <- c(mean(df$lon, na.rm=T), mean(df$lat, na.rm=T))         
map<- get_googlemap(center = cen, maptype = "roadmap", zoom = 10, marker = gc)
ggmap(map)




```






관광지 이름들 나열.
원래 파일 쭉 나열한 다음(특수문자 및 공백 없애고), 들어간거 횟수 체크




문자열을 입력받아서, 같은 문자가 연속적으로 반복되는 경우에 그 반복 횟수를 표시하여 문자열을 압축하기.


입력 예시: aaabbcccccca


출력 예시: a3b2c6a1
```{r}
v<-"ddaaabbbbcc"
v1<-unlist(strsplit(v,split = character(0)))
v2<-unique(v1)
r<-rep(NA, length(v2))
k<-1
j<-1
r[1]<-v2[v2==v1[1]]
v2==v1[1]
r
if(v1[2]==v1[1]){
  k<-k+1
}else{
  j<-j+1
  r[1]<-paste0(r[1],k)
  r[j]<-v2[v2==v1[j]]
  k<-1
}
if(v1[3]==v1[2]){
  k<-k+1
}else{
  r[j]<-paste0(r[j],k)
  j<-j+1
  r[j]<-v2[v2==v1[j]]
  k<-1
}
r[3]<-paste0(r[3],k)
paste0(r[1],r[2])
k
r
```




________________




text<-c('sql','SQL','Sql100','PLSQL','plsql','R','r','r0','python','PYTHON','PythOn','Python#')
grep('sql',text)
text2<-c('sql','r')
grep(text2, text)                 #오류남.
grep(paste(text2, collapse='|'),text,value=T)        #팁.
grep('[0-9]',text, value=T)
grep('[[:digit:]]', text,value=T)
grep('[[:upper:]]', text,value=T)
grep('[[:lower:]]', text,value=T)
grep('[[:alpha:]]', text,value=T)
grep('[[:alnum:]]', text,value=T)
grep('[[:punct:]]', text,value=T)        #구두점 표현


**미리의 방법(plsql, python으로도 해보기)        #0이 한번만 들어가는 문자 찾기
text<-c('sql','SQL','Sql100','PLSQL','plsql','R','r','r0','python','PYTHON','PythOn','Python#')
res <- NA; z<-1
for(i in 1:NROW(text)){
   count <- 0
   tmp <- unlist(strsplit(text[i],split=character(0)))
   for(j in 1:NROW(tmp)){
      if(tmp[j]==0){ count <- count+1 }
   }
   if(count == 1) { res[z] <- text[i]
          z <- z+1 }
}
res
stringr 패키지.
library(stringr)
text<-c('sql','SQL','Sql100','PLSQL','plsql','R','r','r0','python','PYTHON','PythOn','Python#')
# str_detect() : 특정 문자가 있는지를 검사해서 있으면 T, 없으면 F를 출력.
text[str_detect(text, 'sql')        ]        # sql 단어 찾기
text[str_detect(text, '^s')]                # s로 시작하는 단어 찾기
text[str_detect(text, 'n$')]        # n으로 끝나는 단어 찾기
text[str_detect(text, '^[sS]')]        # 시작하는 글자가 소문자 s, 대문자S인 단어 찾기
text[str_detect(text, '[qQ]')]
text[str_detect(text,ignore.case('s'))]
# str_count() : 주어진 단어에서 해당 글자가 몇번 나오는지 알려주는 함수
str_count(text, ignore.case('s'))
str_count(text, ignore.case('l'))
# str_c() : 문자열 합쳐서 출력하는 함수(paste())
str_c("R","빅데이터분석") 
str_c("프로그램 언어: ", text)
str_c(text)
str_c(text, collapse="")
str_c(text,"은 데이터 분석 하기 위해 좋은 언어는 ", text, " 이다.")
# str_dup() : 주어진 문자열을 주어진 횟수만큼 반복해서 출력하는 함수
str_dup(text, 2)
str_c("프로그램 언어 : ", str_dup(text,2))
# str_length() : 주어진 문자열의 길이를 출력하는 함수
str_length(text) 
# str_locate() : 주어진 문자열에서 특정 문자가 처음 나오는 위치와 마지막 위치를 출력하는 함수
str_locate('january','a')
str_locate_all('january','a')
str_locate_all('jaaaaaaaaanuary','aaa')
str_locate_all('jaaaaaaaaanuary','aa..n')
str_locate(text,'sql')
# str_replace() : 주어진 문자열에서 변경전 문자를 변경후 문자로 바꾸는 함수
str_replace('빅데이터분석','빅데이터','가치')
sub('빅데이터','가치','빅데이터분석') 이랑 같은 듯.
str_replace('banana','a','*')
str_replace_all('banana','a','*')
# str_split() : 주어진 데이터셋에서 지정된 기호를 기준으로 분리하는 함수
str <- str_c('sql','/','plsql','/','r')
str
str_split(str,'/')
# str_sub() : 주어진 문자열에서 지정된 길이 만큼의 문자를 잘라내는 함수
str_sub(text, start=1, end=3)
str_sub(text, start=5, end=9)
str_sub(text, start=-2)
# str_trim() : 접두, 접미 부분에 공백문자 제거하는 함수
‘      R     ‘
str_trim(‘      R     ‘)


grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
     fixed = FALSE, useBytes = FALSE, invert = FALSE)
grepl(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)
sub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE)
gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
     fixed = FALSE, useBytes = FALSE)
regexpr(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
gregexpr(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
regexec(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)




________________




데이터프레임에서 na값이 들어간 위치를 알려면 다음과 같이 알 수 있는 것 같음(b+1, a+1) 근데 왜그럴까?
```{r}
a<-which(is.na(emp))%/%nrow(emp)
b<-which(is.na(emp))%%nrow(emp)
length(emp[cbind(b+1,a+1)])
```
참고(여기는  하나 빠짐)
emp[is.na(emp$DEPARTMENT_ID)|is.na(emp$COMMISSION_PCT),]


________________




library(colorspace)
library(jpeg)
library(animation)
library(ggplot2)
nat2013 <- read.csv(paste0('desktop/r/2013nat.csv'),
                    header=T, stringsAsFactors = F, fileEncoding = 'euc-kr')
y<-as.matrix(nat2013[,c(3:ncol(nat2013))])
y[y=='-'|is.na(y)]<-0
y<-as.numeric(y)
nat2013[,c(3:ncol(nat2013))]<-y
names(nat2013)<-c("Major Category",
                  "Small Category", "Korea", "United States",
                  "China", "Japan", "MongGo", "Vietnam",
                  "Thailand", "Taiwan", "Russia", "Bangladesh",
                  "Sri Lanka", "Uzbekistan", "Indonesia", "Canada",
                  "Kyrgyzstan", "Philippines", "Pakistan", "Other")
x2<-c("Violent crime", "theft", "violent crime2",
                       "intelligent crime", "Custom Crime",
                       "Special Economic Crime", "Drug Crime",
                       "Health Crime", "environmental crime",
                       "transportation crime", "labor crime",
                       "security crime", "election crime",
                       "military service crime", "other")
nat2013$`Major Category`<-trimws(nat2013$`Major Category`)
x1<-unique(nat2013[,1])
x3<-unique(nat2013$`Small Category`)
x4<-c("Murder rider", "attempted murder",
      "Robbery", "rape",
      "Rape similar", "forced rape",
      "Others (forced rape)", "Fire",
      "Theft", "Shanghai",
      "Assault", "arrest arrest",
      "Intimidation", "detention",
      "Acts of violence, etc.", "blackmail",
      "Handicap", "Job Organic",
      "Abuse of Authority", "Increase",
      "Call", "document imprint",
      "Securities", "Fraud",
      "Embezzlement", "Sexual crime", "Gambling crime",
      "Special Economic Crimes", "Drug Crimes",
      "Health crime", "Environmental crime",
      "Transportation crime", "Labor crime",
      "Security Crime", "Electoral Crime",
      "Military service crime", "other")


for(i in 1:length(x1)){
  nat2013$`Major Category`[nat2013$`Major Category`==x1[i]]<-x2[i]
}
for(i in 1:length(x3)){
  nat2013$`Small Category`[nat2013$`Small Category`==x3[i]]<-x4[i]
}
n1<-cbind(nat2013[,1:2], count = apply(nat2013[,c(3:ncol(nat2013))], 1, sum))
n1
ggplot(n1, aes(x=`Major Category`, y=count, fill = `Major Category`))+
        geom_bar(stat="identity", width = .5)+
  coord_flip()
n2<-aggregate(count~`Major Category`, n1, sum)


ggplot(n2, aes(x=`Major Category`, y=count, fill = `Major Category`))+
        geom_bar(stat='identity', width = .5)+
        geom_text(aes(label=count), hjust=0, size=2.5)+
  labs(caption='crimes in 2013')+
  theme(axis.text.x=element_text(angle=90), 
        panel.background = element_rect(fill="grey"),
        panel.border = element_rect(linetype=3, fill=NA),
        legend.box.background = element_rect(),
        legend.text = element_text(size=5),
        legend.title = element_text(size=6))+
        coord_trans(y="sqrt")






검찰청/경찰청(또는 행안부) 사건사고 통계들 유형들 봐보기. 시간에 따라 어떻게 변하고 있는지. 대법원 재판 관련 통계도 보기. 
```{r}
library(colorspace)
library(jpeg)
library(animation)
nat2013 <- read.csv(paste0('desktop/r/2013nat.csv'),
                    header=T, stringsAsFactors = F, fileEncoding = 'euc-kr')
nat2014 <- read.csv(paste0('desktop/r/2014nat.csv'),
                    header=T, stringsAsFactors = F, fileEncoding = 'euc-kr')
nat2015 <- read.csv(paste0('desktop/r/2015nat.csv'),
                    header=T, stringsAsFactors = F, fileEncoding = 'euc-kr')
nat2016 <- read.csv(paste0('desktop/r/2016nat.csv'),
                    header=T, stringsAsFactors = F, fileEncoding = 'euc-kr')


y<-as.matrix(nat2013[,c(3:ncol(nat2013))])
y[y=='-'|is.na(y)]<-0
y<-as.numeric(y)
nat2013[,c(3:ncol(nat2013))]<-y
r<-apply(nat2013[,c(3:ncol(nat2013))], 2, sum)
names(r)<-c("Korea", "USA", "China", "Japan",
            "Mongolia", "Vietnam", "Thailand", 
            "Taiwan", "Russia", "Bangladesh", 
            "Sri Lanka", "Uzbekistan", "Indonesia",
            "Canada", "Kirkistan", "Philippines", "Pakistan", "Other")
jpeg('desktop/r/2013.jpeg', width=1380, height=780)
barplot(r, names.arg = names(r), log="y", col = diverge_hcl(ncol(nat2013)-2,c=100, l=c(50,90), power=1),
        ylim=c(1, max(r)*60), las=2, border=NA,
        cex.axis = 1, cex.names = 1, main = '2013', cex.main=1.4)
abline(h=10^seq(0, log(max(r))+1, by=1), lty=3, lwd=.5)
legend("topright", legend = names(r), fill=diverge_hcl(ncol(nat2013)-2,c=100, l=c(50,90), power=1), cex=1, 
       ncol = 2, borde = NA)
dev.off()


y<-as.matrix(nat2014[,c(3:ncol(nat2014))])
y[y=='-'|is.na(y)]<-0
y<-as.numeric(y)
nat2014[,c(3:ncol(nat2014))]<-y
r<-apply(nat2014[,c(3:ncol(nat2014))], 2, sum)
names(r)<-c("Korea", "USA", "China", "Japan", "Mongolia", "Vietnam", "Thailand",
            "Taiwan", "Russia", "Bangladesh", "Sri Lanka", "Uzbekistan", "Indonesia", "Canada",
            "Kirkistan", "Philippines", "Pakistan", "Other")
jpeg('desktop/r/2014.jpeg', width=1380, height=780)
barplot(r, names.arg = names(r), log="y", col = diverge_hcl(ncol(nat2014)-2,c=100, l=c(50,90), power=1),
        ylim=c(1, max(r)*60), las=2, border=NA,
        cex.axis = 1, cex.names = 1, main = '2014', cex.main=1.4)
abline(h=10^seq(0, log(max(r))+1, by=1), lty=3, lwd=.5)
legend("topright", legend = names(r), fill=diverge_hcl(ncol(nat2014)-2,c=100, l=c(50,90), power=1), cex=1, 
       ncol = 2, borde = NA)
dev.off()


y<-as.matrix(nat2015[,c(3:ncol(nat2015))])
y[y=='-'|is.na(y)]<-0
y<-as.numeric(y)
nat2015[,c(3:ncol(nat2015))]<-y
r<-apply(nat2015[,c(3:ncol(nat2015))], 2, sum)
names(r)<-c("Korea", "USA", "China", "Japan", "Mongolia", "Vietnam", "Thailand",
            "Taiwan", "Russia", "Bangladesh", "Sri Lanka", "Uzbekistan", "Indonesia", "Canada",
            "Kirkistan", "Philippines", "Pakistan", "Other")
jpeg('desktop/r/2015.jpeg', width=1380, height=780)
barplot(r, names.arg = names(r), log="y", col = diverge_hcl(ncol(nat2015)-2,c=100, l=c(50,90), power=1),
        ylim=c(1, max(r)*60), las=2, border=NA,
        cex.axis = 1, cex.names = 1, main = '2015', cex.main=1.4)
abline(h=10^seq(0, log(max(r))+1, by=1), lty=3, lwd=.5)
legend("topright", legend = names(r), fill=diverge_hcl(ncol(nat2015)-2,c=100, l=c(50,90), power=1), cex=1, 
       ncol = 2, borde = NA)
dev.off()


y<-as.matrix(nat2016[,c(4, c(6:ncol(nat2016)))])
nat2016
y[y=='-'|is.na(y)]<-0
y
y<-as.numeric(y)
nat2016[,c(4, c(6:ncol(nat2016)))]<-y
r<-apply(nat2016[,c(4, c(6:ncol(nat2016)))], 2, sum)
names(r)<-c("Korea", "USA", "China", "Japan", "Mongolia", "Vietnam", "Thailand",
            "Taiwan", "Russia", "Bangladesh", "Sri Lanka", "Uzbekistan", "Indonesia", "Canada",
            "Kirkistan", "Philippines", "Pakistan", "Other")
jpeg('desktop/r/2016.jpeg', width=1380, height=780)
barplot(r, names.arg = names(r), log="y", col = diverge_hcl(ncol(nat2016)-2,c=100, l=c(50,90), power=1),
        ylim=c(1, max(r)*60), las=2, border=NA,
        cex.axis = 1, cex.names = 1, main = '2016', cex.main=1.4)
abline(h=10^seq(0, log(max(r))+1, by=1), lty=3, lwd=.5)
legend("topright", legend = names(r), fill=diverge_hcl(ncol(nat2016)-2,c=100, l=c(50,90), power=1), cex=1, 
       ncol = 2, borde = NA)
dev.off()


for(j in 1:100){
for(i in 2013:2016){
  img<-paste0("desktop/r/",i, ".jpeg")
  img<-readJPEG(img)
  plot.new()
  rect(0,0,1,1,col=NA, border=NA)
  rasterImage(img,0,0,1,1)
  ani.pause(.1)
}
}








```



