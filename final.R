#this is the base file
library(MASS)
attach(painters)

#this sums each painter
painter_sums <- data.frame(rowSums(painters[,1:4]))
colnames(painter_sums) <- c('sum')

#sum of scores by each school
aggregate(. ~ School, painters, sum)

# need to do a t-test