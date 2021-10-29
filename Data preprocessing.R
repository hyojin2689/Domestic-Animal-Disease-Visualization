#Package Used
library(dplyr)
library(tidyverse)
library(ggplot2)

#Preprocess Data
#결측치 확인
is.na(df)
sum(is.na(df))

#결측치 제거: 종식일 칼럼 제거
df <- subset(df, select = -종식일)
str(df)
sum(is.na(df))

head(df,n=20)

#가축전염병명: 띄어쓰기 제거
df$가축전염병명 <- gsub(' ','', df$가축전염병명 )

#가축전염병명: 가금티푸스, 가금티프스 같은 병명임으로 획일화
df$가축전염병명 <- gsub('가금티프스','가금티푸스', df$가축전염병명 )

#축종.품종. 소분류까지 확인
df$축종.품종. %>% unique() %>% sort()

#축종.품종. 대분류까지 확인
class_df = df$축종.품종. %>% strsplit(split = '-') 
class_df

class <- c()
for(i in 1:length(class_df)){
  class <- c(class, class_df[[i]][1])
}

#대품종 카테고리 확인
classnames <- class %>% unique() %>% sort()

#데이터 대분류 칼럼 생성: class이름 칼럼으로 붙여주고, ' '으로 들어간 행 삭제
df$대품종 <- class
df <- df[-grep(" ",df$대품종),]

#df에 년, 월, 일로 나누어 칼럼생성
df <- transform(df, yy = substr(df$발생일자.진단일.,1,4))
df <- transform(df, mm = substr(df$발생일자.진단일.,5,6))
df <- transform(df, dd = substr(df$발생일자.진단일.,7,8))

#state 칼럼 추가 
map_df = df$농장소재지 %>% strsplit(split = ' ') 
map_df

map <- c()
for(i in 1:length(map_df)){
  map <- c(map, map_df[[i]][1])
}

df$state <- map
