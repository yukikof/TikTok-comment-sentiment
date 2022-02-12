#clean code
library(rvest)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(lubridate)


#import csv - wd can be changed but probably best if on system/external usb
setwd("C:\\Users\\yukik\\Documents\\R\\TikTok-Comment-Sentiment") 

#anonymisation


anonymise <- function(f){
df <- read.csv(f)
colnames(df)[1] <-"ID" 
df <- df %>% select("ID","Date","Likes","Comment")
write.csv(df,f,append = FALSE,sep = ",")
}

anonymise(filename) #for single file

filename <-"filter616b34bc62655_csv.csv"
filenames <- list.files(pattern="*.csv")

tapply(filenames,anonymise)

comments <- read.csv(filename) 



#defining blank output and emojis
outputs <- data.frame()
emojis <- "\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff]"
#timestamps <- "\\d*:?\\d{1,2}:\\d{2}"


#function:sentimentanalysis
sentimentanalysis <- function(v){
  

  coms <- comments %>%
    arrange(desc(Date)) %>% 
    tibble()
  if (nrow(coms)>100){
  coms <- coms[1:100,]}
  
  #remove comments object to free up space
  #remove(comments)
  
  #separates into tokens, in this case each word
  tokenword <- coms %>% unnest_tokens(word,Comment,token = "words")
  
  #remove stop words (common words such as I, it, is)
  tokens<- tokenword %>% 
    mutate(word=str_replace_all(word,timestamps,"")) %>% 
    mutate(word=str_replace_all(word,emojis,"")) %>% 
    filter(!word %in% stop_words$word& !str_detect(word,"^\\d+$")) 
  
  #using bing sentiments which scores key by positive or negative
  afinn <- get_sentiments("afinn") %>%
    #mutate(sentiment=str_replace_all(sentiment,"negative",'-1')) %>% 
    #mutate(value=str_replace_all(sentiment,"positive",'1')) %>% 
    mutate(value=as.numeric(value)) #stringr only accepts characters so needs converting
  
  #mean, median, range of sentiment scores grouped by comment
  coms_sa <- tokens %>% 
    left_join(bing,by="word") %>% 
    group_by(ID) %>% 
    na.omit() %>% 
    summarise(mean=mean(value),median=median(value)) 
  
 # outputs <- rbind(outputs,c(mean(coms_sa$mean),
  #                           median(coms_sa$median))) %>% 
   # setNames(c("mean","median"))
  
  return(coms_sa)
  
}

#this outputs the mean sentiment of each comment
x <- sentimentanalysis(comments) %>% tibble()
summary_stats <- x %>% summarise(mean=mean(x$mean),median=median(x$median)) %>% data.frame()

write.table(summary_stats,file = "test.csv",append = TRUE,sep = ",",quote = FALSE,col.names = FALSE)



#analysis----
#results <- sapply(video_ids,sentimentanalysis) %>% t() %>% 
#  as.data.frame() %>% 
#  cbind(rownames(results), results)
#rownames(results1) <- NULL
#colnames(results1) <- c("id","mean","median","medianLikes","meanWeighted")

#stats <- sapply(video_ids,get_stats) %>%  
#  as.data.frame() %>% t() %>% as.data.frame() %>% 
#  mutate(id=.$id %>% as.character())


#function:applyanalysis doesnt work


applyanalysis(video_ids)
obj <- applyanalysis(searchCut_ids)
#obj2 <- sapply(searchCut_ids, sentimentanalysis)

#more experimental stuff----

withs_stats_object <- left_join(results1,stats1) 
withs_stats_object %>% 
  mutate(likeCount=as.numeric(unlist(.$likeCount)),
         dislikeCount=as.numeric(unlist(.$dislikeCount)),
         mean=as.numeric(unlist(.$mean)),
         percentage=likeCount*100/(dislikeCount+likeCount)) %>% 
  ggplot(aes(x=percentage,y=mean))+
  geom_point()+
  ylab("mean sentiment")+
  xlab("like ratio as percentage of total")

#graph plotting
ggplot(coms_sa,aes(mean))+
  geom_histogram()

ggplot(coms_sa,aes(median))+
  geom_histogram()

ggplot(coms_sa,aes(weighted))+
  geom_histogram()+
  scale_x_log10()+
  ggtitle("weighted sentiment analysis",video_id)


ggplot(coms_sa,aes(x=likeCount,y=mean))+
  geom_point()+
  ylab("mean comment sentiment")+
  scale_fill_gradient(low="red",high = "yellow")+
  ggtitle("comment sentiment vs likes",video_id)


comments %>% 
  mutate(likeCount=as.numeric(likeCount)) %>% 
  top_n(10,likeCount) %>% 
  select(textOriginal,likeCount) %>% 
  arrange(desc(likeCount)) %>% 
  View()

coms_users %>% 
  ungroup() %>% 
  select(word,value) %>% 
  count(word) %>% 
  top_n(10,n) %>% 
  mutate(word= reorder(word,n)) %>% 
  arrange(desc(n))

get_video_details(video_id = video_id)
cmnt_disabled <- "cRpdIrq7Rbo"
cmnt_stats<- get_stats(video_id = cmnt_disabled)
cmnt_stats$commentCount==0
cmnt_stats$dislikeCount<200


sapply(searchCut_ids,sentimentanalysis)
statsapply <- sapply(searchCut_ids,get_stats)
statsapply <- statsappky %>% t() %>% 
  data.frame() %>% 
  filter(.$commentCount>0)
sentiments <- sapply(statsapply$id,sentimentanalysis)
