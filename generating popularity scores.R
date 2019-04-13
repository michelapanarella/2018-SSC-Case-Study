#Generating popularity scores

#Michela

#May 8th 2018

#set the wd and upload the data
setwd()
library(stringr)
ted <- read.csv('ted_main.csv')

#extract the number of ratings 
rate <- strsplit(as.character(ted$ratings),'},')
name <- matrix(0, nrow=14, ncol=2550); 
count<- matrix(0, nrow=14, ncol=2550);

#ugly for loop but gets the job done
for(i in 1:2550){
  for(j in 1:14){
    unlist_rate <- unlist(rate[[i]])
    ind_rate <- strsplit(unlist_rate[j], split=",")
    label_rate <- strsplit(ind_rate[[1]][2], split=":")
    num <- str_extract_all(ind_rate[[1]][3], "[0-9]+")
    count[j,i] <- as.numeric(num);  
    name[j,i] <- noquote(label_rate[[1]][2])
  }
}
name <- as.data.frame(name)

#re-order the ratings such that the rating order is identical 
for(i in 1:2550){
  count[,i] = count[order(name[,i]),i]
}
outcome <- rbind(count/3,ted$views,ted$comments)
row.names(outcome) = c("Beautiful", "Confusing", "Courageous", "Fascinating",
                          "Funny","Informative","Ingenious","Inspiring",
                          "Jaw-dropping", "Longwinded","Obnoxious", "OK",
                          "Persuasive", "Unconvincing", "views", "comments")
outcome <- as.data.frame(outcome)

#########################

#create the popularity score

#factors have equal weights; the boring factor is downweighted
weight_boring = -0.5
weight_admirable = 0.9
weight_exciting = 0.1
weight_debatable = 0.5
exciting <- as.numeric(t(colSums(outcome[c("views","Fascinating","Funny","Ingenious","Jaw-dropping"),]/5)*weight_exciting))
admirable <- as.numeric(t(colSums(outcome[c("Beautiful","Courageous", "Inspiring" ),]/3)*weight_admirable))
debatable <- as.numeric(t(colSums(outcome[c("comments","Informative", "Persuasive"),]/3)*weight_debatable))
boring <- as.numeric(t(colSums(outcome[c("Confusing","Longwinded","Obnoxious","OK","Unconvincing"),]/5)*weight_boring))

  
scores <-  data.frame(exciting, admirable, debatable, boring)
scores$popularity_index <- with(scores, rowSums(scores))

par(mfrow=c(1,2))
hist(scores$popularity_index, main="Histogram of popularity scores")
hist(log(scores$popularity_index), main="Histogram of log popularity scores")

#exploratory daya analysis