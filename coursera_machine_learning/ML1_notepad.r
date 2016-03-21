library("kernlab")
data(spam)

plot(density(spam$your[spam$type=="nonspam"]),col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
abline(v=0.5,col="black")
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction,spam$type)/length(spam$type)

set.seed(333)
#Take 10 messages
smallSamp <- spam[sample(dim(spam)[1],size=10),]
spamLabel <- (smallSamp$type=="spam") *1 + 1
# Average number of capital letters
plot(smallSamp$capitalAve, col=spamLabel)
# If you have lot of capitals then it's a spam
#capitalAve > 2.7 then Spam 
#capitalAve < 2.4 then nonspam
#capitalAve between 2.4 and 2.45 then spam
#capitalAve between 2.45 and 2.7 then nonspam

rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.4] <- "nonspam"
  prediction[x >= 2.4 & x <= 2.45] <- "spam"
  prediction[x >= 2.45 & x <= 2.7] <- "nonspam"
  return (prediction)
}

table(rule1(smallSamp$capitalAve),smallSamp$type)

rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return (prediction)
}

table(rule2(smallSamp$capitalAve),smallSamp$type)

# on complete data set
table(rule1(spam$capitalAve),spam$type)
table(rule2(spam$capitalAve),spam$type)

#Accuracy
sum(rule1(spam$capitalAve) == spam$type)
sum(rule2(spam$capitalAve) == spam$type)