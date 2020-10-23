# Exercise 8

setwd("/Users/Maddie Galvin/Desktop/Biocomputing2020_Tutorial10/")
library(tibble)
library(dplyr)
# 1. plot sports scores
df <- read.table(file="UWvMSU_1-22-13.txt",header=TRUE,sep="", stringsAsFactors=FALSE)
#sportsMatrix <- as.matrix(df)
#UWscore = sum(df$score[df$team=="UW"])
#MSUscore = sum(df$score[df$team=="MSU"])
time = df$time
score = df$score

UWscore = 0
MSUscore = 0
for (i in 1:length(df$time)){
  if (df$team[i]=="UW"){
    UWscore = UWscore + df$score[i]
  }
  else {
    MSUscore = MSUscore + df$score[i]
  }
}

UWscore
MSUscore

for (i in 1:length(df$time)){
  df["UWscore"] <- UWscore[i]
  df["MSUscore"] <- MSUscore[i]
}

df

plot(time,UWscore,type='l', col="blue")
lines(time, MSUscore, type='l', col="red")

## new solution

UWpt=0
MSUpt=0
df["UWscore"]=UWpt
df["MSUscore"]=MSUpt
for (i in 1:length(df$time)){
  if (df$team[i]=="UW"){
    UWpt[i]=UWpt+df$score[i]
    df$UWscore[i]=UWpt[i]
  } else {
    MSUpt[i]=MSUpt+df$score[i]
    df$MSUscore[i]=MSUpt[i]
  }
}

##beth solution


UWscore = 0
MSUscore = 0

for (i in 1:length(df$time)){
  if (df$team[i]=="MSU"){
    if (df$time[i]==4.833333){
      MSUscore=2
    } else{
      df[is.na(MSUscore)]=0
      df <- df%>%
        mutate(MSUscore=score+lag(MSUscore))
    }} else {
      if (df$time[i]==3.966667){
        UWscore=3
      } else{
        df[is.na(UWscore)]=0
        df <- df%>%
          mutate(UWscore=score+lag(UWscore))
      }
    }
}

for (i in 1:length(df$time)){
  if (df$team[i]=="MSU"){
    df[is.na(MSUscore)]=0
    df <- df%>%
      mutate(MSUscore=score+lag(MSUscore))
  }
  else {
    df[is.na(UWscore)]=0
    df <- df%>%
      mutate(UWscore=score+lag(UWscore))
  }
}


# 2. guess my number
guessMyNumber <- function(answer, guess){
  answer <- sample(1:100, 1, replace=TRUE)
  guessCount = 0
  guessLimit = 9
  while (guessCount <= guessLimit){
    guess <- readline("Guess a number: ")
    guessCount = guessCount + 1
    if (answer>guess){
      print("Higher")
    } else if (answer<guess){
      print("Lower")
    } else {
      print("Correct!")
    }
  }
}

# run this code to play in console
guessMyNumber(answer, guess)
