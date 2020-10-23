# Galvin - DiDonato Exercise 8 

setwd("/Users/Maddie Galvin/Desktop/Biocomputing2020_Tutorial10/")

### 1. plot sports scores

df <- read.table(file="UWvMSU_1-22-13.txt",header=TRUE,sep="", stringsAsFactors=FALSE)
#sportsMatrix <- as.matrix(df)

time = df$time
score = df$score

# we got so close to solving this question, but we couldn't make
# the cumulative score work right. Here's our best solution:

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

plot(time,UWscore,type='l', col="blue")
lines(time, MSUscore, type='l', col="red")

# here's a second possible solution:
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

plot(time,UWscore,type='l', col="blue")
lines(time, MSUscore, type='l', col="red")


### 2. guess my number
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
