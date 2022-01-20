setwd("C:/Users/James Xia/Desktop/STAT4060J/project")
library(tidyverse)
library(ggplot2)
shootings <- read.csv("fatal-police-shootings-data.csv")
col_base <- c("id", 
              "name", 
              "date", 
              "manner_of_death", 
              "armed", 
              "age", 
              "race", 
              "city", 
              "state", 
              "signs_of_mental_illness", 
              "threat_level", 
              "flee", 
              "body_camera", 
              "longitude", 
              "latitude", 
              "is_geocoding_exact"
              )
#colnames(shootings) <- col_base
shootings$date
# g <- group_by(shootings, date)
ggplot(shootings, aes(x = date)) + geom_histogram(stat = "count", col = "blue")
dim(shootings)[1]

num <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
count <- 1
days <- 0
for(i in 4929:dim(shootings)[1]){
  if(i == 4929){
    days <- days + 1
    next
  }
  
  if(shootings$date[i] == shootings$date[i-1]){
    count <- count + 1
    if(i == dim(shootings)[1]){
      num[count + 1] <- num[count + 1] + 1
      break
    }
    next
  }
  else {
    num[count + 1] <- num[count + 1] + 1
    days <- days + 1
    if(i == dim(shootings)[1]){
      num[2] <- num[2] + 1
      break
    }
    count <- 1
    next
  }
  
}
num
days
num[1] <- 366 + 191 - days
num[1]
x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- num
data <- c()
for(i in 1:11) {
  data <- c(data, rep(i - 1, num[i]))
}
data
hist(data, right = FALSE, xlab = "shooting numbers", ylab = "number of days", xlim = c(0, 10), ylim = c(0, 200), col = "yellow")
plot(density(data))

index <- 0
for(i in 1:dim(shootings)[1]){
  if(shootings$date[i] == "2021/1/1") break
  index <- index + 1
}
index
dim(shootings)[1]
