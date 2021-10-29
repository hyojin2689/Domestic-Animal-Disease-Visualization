#Package Used
library(dplyr)
library(tidyverse)
library(ggplot2)

1.대품종별 가출전염병에 걸린 두 수
#분석을 위한 d1 데이터 프레임 생성
d1 <- df  %>%
  select('가축전염병명', '대품종', '발생두수.마리.') %>% 
  group_by(대품종,가축전염병명) %>% summarise(sum = sum(발생두수.마리.)) %>%
  arrange(대품종)
d1

1-1.대품종별 가출전염병에 걸린 두 수
p1 <- ggplot(data = d1, mapping = aes(x=대품종, y=sum, fill=가축전염병명))
p1 + geom_col(position = 'dodge', width=0.8) + scale_y_log10() + 
  geom_text(aes(label = sum), vjust = 1, color = "black") + 
  labs(title = '대품종별 가출전염병에 걸린 두 수',
       x = '두 수',
       y = '가축 전염병명') +  theme(axis.title=element_text(size=17),title=element_text(size=20)) 

1-2.대품종별 가출전염병에 걸린 두 수
p1 <- ggplot(data = d1, mapping = aes(x=reorder(가축전염병명,sum), y=sum, fill=가축전염병명))
p1 + geom_col(position = 'dodge', width=0.8) + scale_y_log10() + 
  geom_text(aes(label = sum), vjust = 1, color = "black") + 
  labs(title = '대품종별 가출전염병에 걸린 두 수',
       x = '두 수',
       y = '가축 전염병명')+coord_flip()+facet_wrap(~대품종,ncol=10)+
       theme(axis.title=element_text(size=17),title=element_text(size=20))  
       
1-3.가축 전염병별 감염된 대품종의 비율
p1 <- ggplot(data = d1, mapping = aes(x=가축전염병명, y=sum, fill=대품종))
p1 + geom_bar(stat='identity',position="fill") +
  labs(title = '가축 전염병별 감염된 대품종의 비율',
       x = '대품종',
       y = '두 수')+coord_flip()+
       theme(axis.title=element_text(size=17),title=element_text(size=20))

2.최근 5년/10년 사이의 월별 평균 발생 두 수 
2-1.2016년부터 2020년까지 최근 5년간 월별 평균 마리수
d2 <- df  %>% select('yy', 'mm', '발생두수.마리.') %>% 
  subset(yy>=2016 & yy<=2020) %>%
  group_by(mm) %>% summarise(mean = mean(발생두수.마리.)) 

p2 <- ggplot(data = d2, mapping = aes(x=mm, y=mean, group = 1))
p2 + geom_line() + geom_point(size = 2) + 
  labs(title = '최근 5년(2016~2020) 사이의 월별 평균 발생 두수',
       x = '월',
       y = '평균 두 수')+ theme(axis.title=element_text(size=17),title=element_text(size=20))

2-2.2011년부터 2020년까지 최근 10년간 월별 평균 마리수
d3 <- df  %>% select('yy', 'mm', '발생두수.마리.') %>% 
  subset(yy>=2011 & yy<=2020) %>%
  group_by(mm) %>% summarise(mean2 = mean(발생두수.마리.)) 

p3 <- ggplot(data = d3, mapping = aes(x=mm, y=mean2, group = 1))
p3 + geom_line() + geom_point(size = 2) + 
  labs(title = '최근 10년(2011~2020) 사이의 월별 평균 발생 두수',
       x = '월',
       y = '평균 두 수')+ theme(axis.title=element_text(size=17),title=element_text(size=20))  

2-1과 2-2를 한 번에 시각화 2011년부터 2020년까지 최근 10년간 월별 평균 마리수
#d2,d3 월을 기준으로 합치기
d4 <- merge(d2,d3,by="mm")

p4 <- ggplot(d4,aes(x=mm,y=mean,group=1))
p4+geom_line(size=1,color="blue")+geom_line(aes(x=mm,y=mean2,group=1),size=1,color="red")+
  labs(title = '최근 5/10년(2016~2020/2011~2020) 사이의 월별 평균 발생 두수',x = '월',y = '평균 두 수')+
  annotate(geom="text",x=03.3,y=1600,label="2011-2020",hjust=0)+
  annotate(geom="text",x=03,y=200,label="2016-2020",hjust=0)+
  theme(axis.title=element_text(size=17),title=element_text(size=20))   

3.지역별 가축 전염병에 걸린 두 수
d5 <- df  %>%
  select('state', '발생두수.마리.') %>% 
  group_by(state) %>% summarise(sum = sum(발생두수.마리.)) %>%
  arrange(sum)
d5

p5 <- ggplot(data = d5, mapping = aes(x=reorder(state,sum), y=sum,fill=as.factor(state)))
p5 + geom_bar(position = 'dodge', width=0.8,stat='identity') + scale_y_log10() + 
  geom_text(aes(label = sum), vjust = -1, color = "black") + 
  labs(title = '지역별 가출전염병에 걸린 두 수',x = '지역',y = '두 수')+
  theme(axis.text.x=element_text(angle=45),axis.title=element_text(size=17),title=element_text(size=20))+
  guides(fill=F)

4.최근 10년(2011~2020) 사이의 월별 평균 가축 대품종별 발생 두수
d5 <- df  %>% select('yy', 'mm', '대품종','발생두수.마리.') %>% 
  subset(yy>=2011 & yy<=2020) %>%
  group_by(mm,대품종) %>% summarise(mean = mean(발생두수.마리.)) 

bigcategory_labels <- df$대품종 %>% unique() %>% sort()
mm_labels <- df$mm %>% unique() %>% sort()

#월 데이터 생성
mm_sum <- c()
for(i in 1:length(bigcategory_labels)){
  mm_sum = append(mm_sum,mm_labels)
}
mm_sum %>% sort()

#대품종 데이터 생성
bigcategory_sum <- c()
for(i in 1:length(mm_labels)){
  bigcategory_sum = append(bigcategory_sum,bigcategory_labels)
}
bigcategory_sum

#월,대품종 데이터 생성
subd5 <- data.frame(mm = mm_sum %>% sort(),
                    대품종 = bigcategory_sum)
#월, 대품종 데이터를 merge로 mean값을 넣고, 없는 값은 0으로 대체
d5 <- merge(d5,subd5,by=c('mm','대품종'),all=TRUE)
d5[is.na(d5)] <- 0
d5

p5 <- ggplot(data = d5, mapping = aes(x=mm, y=mean, color = 대품종, group=대품종))
p5 + geom_line() + geom_point(size = 2) + scale_y_log10() + 
  labs(title = '최근 10년(2011~2020) 사이의 월별 평균 가축 대품종별 발생 두수',
       x = '월',
       y = '평균 두 수')+  theme(axis.title=element_text(size=17),title=element_text(size=20))  

p5 + geom_line() + geom_point(size = 2) + scale_y_log10() + 
  labs(title = '최근 10년(2011~2020) 사이의 월별 평균 가축 대품종별 발생 두수',
       x = '월',
       y = '평균 두 수')+facet_wrap(~대품종)+
       theme(axis.title=element_text(size=17),title=element_text(size=20))  

5.최근 10년(2011~2020) 사이의 월별 평균 가축전염병명별 발생 두수
d6 <- df  %>% select('yy', 'mm', '가축전염병명','발생두수.마리.') %>% 
  subset(yy>=2011 & yy<=2020) %>%
  group_by(mm,가축전염병명) %>% summarise(mean = mean(발생두수.마리.)) 
d6

#데이터 개수 확인인
length(df$가축전염병명 %>% unique())

disease_labels <- df$가축전염병명 %>% unique() %>% sort()
mm_labels <- df$mm %>% unique() %>% sort()

#월 데이터 생성
mm_sum <- c()
for(i in 1:length(disease_labels)){
  mm_sum = append(mm_sum,mm_labels)
}
mm_sum

#가축전염병명 데이터 생성
disease_sum <- c()
for(i in 1:length(mm_labels)){
  disease_sum = append(disease_sum,disease_labels)
}
disease_sum

#월,가축전염병명 데이터 생성
subd6 <- data.frame(mm = mm_sum %>% sort(),
                    가축전염병명 = disease_sum)

#월, 가축전염병명 데이터를 merge로 mean값을 넣고, 없는 값은 0으로 대체
d6 <- merge(d6,subd6,by=c('mm','가축전염병명'),all=TRUE)
d6[is.na(d6)] <- 0
d6

p6 <- ggplot(data = d6, mapping = aes(x=mm, y=mean, color = 가축전염병명, group=가축전염병명))
p6 + geom_line() + geom_point(size = 2) + scale_y_log10() + 
  labs(title = '최근 10년(2011~2020) 사이의 월별 평균 가축전염병명별 발생 두수',
       x = '월',y = '평균 두 수') +
  facet_wrap(~가축전염병명) + theme(legend.position = 'bottom')+ 
  theme(axis.title=element_text(size=17),title=element_text(size=20))  

6.지도 그리기
#Package Used
library(httr)
library(jsonlite)
library(ggmap)

register_google(key='google API')

#2016년에서 2020년까지의 필요한 데이터만 추출
d7 <- df  %>% dplyr::select('yy','농장소재지','대품종','가축전염병명','발생두수.마리.','state') %>% 
  subset(yy>=2016 & yy<=2020) %>%mutate(id = row_number()) %>% ungroup()

#x, y인 경도, 위도 값을 넣어주기
id_sum <- c()
map_sum <- c()
x_sum <- c()
y_sum <- c()
for(i in 1:nrow(d7)){
  addr <- d7$농장소재지[i]
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  id_sum <- append(id_sum, i)
  map_sum <- append(map_sum, addr)
  x_sum <- append(x_sum,ifelse(is.null(coord$documents$x[1]),0,coord$documents$x[1]))
  y_sum <- append(y_sum,ifelse(is.null(coord$documents$y[1]),0,coord$documents$y[1]))
}

subd7 <- data.frame(id = id_sum,
                    농장소재지 = map_sum,
                    x = x_sum,
                    y = y_sum)

d7 <- merge(d7,subd7,by=c('id','농장소재지'),all=TRUE)
d7 <- subset(d7, x!=0)
d7 <- subset(d7, y!=0)
d7 <- rename(d7, '행정구역'='state')

kor <- get_map(location = c(127.1717,35.9899),
               zoom = 7, scale = 2)

ggmap(kor) + geom_point(d7, 
                        mapping = aes(x=as.numeric(x),
                                      y=as.numeric(y), 
                                      color = 행정구역),
                        shape = 16, size = 2) +
  theme(axis.title=element_text(size=17),title=element_text(size=20)) +
  labs(title='농장소재지 지도 시각화' , x ='경도', y = '위도')
 
7.농장별 총 두 수 집계 (상위 10개)
d8 <- df[,c(3,13)] #농장명과 대품종 컬럼 추출
colnames(d8)
d8 <- d8 %>% group_by(농장명.농장주.) #농장명별로집계
d8 <- d8 %>% arrange(desc(농장명.농장주.)) #농장명 내림차순 정렬
d8

#중복 농장명 갯수 세기
count_f<-data.frame(table(d8$농장명.농장주.)) 
colnames(count_f)=c("farm_name","count")
count_f<- count_f[c(order(count_f$count)),] #count 내림차순 정렬
count_f<-tail(count_f,n=12) #상위 10개 추출 (확인 농장, 이름없는 농장 제외)
count_f<-count_f[-9,] #확인 농장 제거
count_f<-count_f[c(1:10),] #이름없는 농장 제거
count_f <- count_f %>% arrange(-count)
count_f

p7 <- ggplot(data = count_f, mapping = aes(x=reorder(farm_name,-count), y=count,fill=as.factor(farm_name)))
p7 + geom_bar(position = 'dodge', width=0.8,stat='identity') +coord_flip()+guides(fill=F)+
  geom_text(aes(label = count), hjust = -1, color = "black")+
  labs(title = '농장별 총 두 수 집계',x = '농장명',y = '총 계')+
  theme(axis.title=element_text(size=17),title=element_text(size=20))  

8.상관분석 (상위 10개)
#Package Used
library(corrplot)

#원래 농장에서 각 농장별 가축 마리수 파악하기
d8[d8$농장명.농장주. == '대륙',]
d8[d8$농장명.농장주. == '미산농장',]
d8[d8$농장명.농장주. == '삼형제농장',]
d8[d8$농장명.농장주. == '한우촌2농장',]
d8[d8$농장명.농장주. == '양창조',]
d8[d8$농장명.농장주. == '우리농장',]
d8[d8$농장명.농장주. == '김영호',]
d8[d8$농장명.농장주. == '대성농장',]
d8[d8$농장명.농장주. == '2농장',]
n<-d8[d8$농장명.농장주. == '1농장',]
n<-data.frame(table(n$대품종)) #48마리 보유한 농장 가축 마리수 세기 (중복 행 갯수 세기)
n

#df생성
a<-c(9,0,0,0,0)
b<-c(9,0,0,0,0)
c<-c(9,0,0,0,0)
d<-c(9,0,0,0,0)
e<-c(9,0,0,0,0)
f<-c(7,3,0,0,0)
g<-c(0,0,11,0,0)
h<-c(11,0,0,1,0)
i<-c(13,1,0,0,2)
j<-c(44,3,0,0,1)
d8<-data.frame(rbind(a,b,c,d,e,f,g,h,i,j))
colnames(d8)=c("소","돼지","벌","닭","오리")
rownames(d8)=c("대륙","미산농장","삼형제농장","한우촌2농장","양창조","우리농장","김영호","대성농장","2농장","1농장")
d8
corrplot.mixed(corr=cor(d8[,c("소","돼지","벌","닭","오리")]))

9.고성병원 조류 인플루엔자 바이러스 유입경로
path <- read.csv("inflow_path.csv") 
path <- table(path$inflow_path)
path <- data.frame(path)
colnames(path)=c("inflow_path","count")
path<-path %>% filter(count >= 3)
path

p8 <- ggplot(data = path, mapping = aes(x=reorder(inflow_path,count), y=count,fill=as.factor(inflow_path)))
p8 + geom_bar(position = 'dodge', width=0.8,stat='identity') +coord_flip()+guides(fill=F)+
  geom_text(aes(label = count), hjust = -1, color = "black")+
  labs(title = '고성병원 조류 인플루엔자 바이러스 유입 경로',x = '유입 경로',y = '총 계')+
  theme(axis.title=element_text(size=17),title=element_text(size=20))  
