#This script anonymises the dataset by rewriting the file without identifying 
#information (usernames, unique ID, etc).
#[author: Yukiko F, created 12 Feb 2022]

library(rvest)
library(dplyr)

#import csv - please change working directory depending on where your files are
wd <- "C:\\Users\\yukik\\Documents\\R\\TikTok-Comment-Sentiment"
setwd(wd) 

#anonymisation. Defines a function that overwrites files by a version with 
#only necessary information: comment number(No), date, like count and comment text

anonymise <- function(f){
  df <- read.csv(f)
  colnames(df)[1] <-"No" 
  df <- df %>% select("No","Date","Likes","Comment")
  write.csv(df,f,append = FALSE,sep = ",")
}

#takes relevant files and applies the anonymisation
filenames <- list.files(pattern="filter[0-9A-Za-z]*.csv")
sapply(filenames,anonymise)
