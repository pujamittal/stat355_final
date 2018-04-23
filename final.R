#this is the base file
library(MASS)
attach(painters)
painters

#this sums each painter, does not let you know which group they are in
painter_sums <- data.frame(rowSums(painters[,1:4]))
colnames(painter_sums) <- c('sum')

#average of scores by each school
agg_paint <- aggregate(. ~ School, painters, mean)
?aggregate
# H is highest in Composition and Expression
# A is highest in Drawing
# D is highest in Colour

# Renaissance
groupAd<-subset(painters$Drawing, (painters$School == "A"))
groupA_draw=sample(groupAd,1000,replace = TRUE)
t.test(groupA_draw, alternative = "greater", mu = 10)
#yes
#p-value < 2.2e-16
#mean 14.738

groupAe<-subset(painters$Expression, (painters$School == "A"))
groupA_exp=sample(groupAe,1000,replace = TRUE)
t.test(groupA_exp, alternative = "greater", mu = 10)
#no
#p-value = 1
#mean 8.241

groupAcl<-subset(painters$Colour, (painters$School == "A"))
groupA_colour=sample(groupAcl,1000,replace = TRUE)
t.test(groupA_colour, alternative = "greater", mu = 10)
#no
#p-value = 1
#mean 9.121

# Mannerist
groupBcm<-subset(painters$Composition, (painters$School == "B"))
groupB_comp=sample(groupBcm,1000,replace = TRUE)
t.test(groupB_comp, alternative = "two.sided", mu = 10)
#yes
#p-value < 2.2e-16
#mean 12.178

# Seicento
groupCe<-subset(painters$Expression, (painters$School == "C"))
groupC_exp=sample(groupCe,1000,replace = TRUE)
t.test(groupC_exp, alternative = "greater", mu = 10)
#no
#p-value = 1
#mean 7.029

groupCcl<-subset(painters$Colour, (painters$School == "C"))
groupC_colour=sample(groupCcl,1000,replace = TRUE)
t.test(groupC_colour, alternative = "greater", mu = 10)
#no
#p-value = 1
#mean 7.545

# Venetian
groupDd<-subset(painters$Drawing, (painters$School == "D"))
groupD_draw=sample(groupDd,1000,replace = TRUE)
t.test(groupD_draw, alternative = "less", mu = 10)
#no
#p-value = 0.3863
#mean 9.971

groupDcl<-subset(painters$Colour, (painters$School == "D"))
groupD_colour=sample(groupDcl,1000,replace = TRUE)
t.test(groupD_colour, alternative = "greater", mu = 10)
#yes
#p-value < 2.2e-16
#mean 16.075

# Lombard
groupEcm<-subset(painters$Composition, (painters$School == "E"))
groupE_comp=sample(groupEcm,1000,replace = TRUE)
t.test(groupE_comp, alternative = "greater", mu = 10)
#yes
#p-value < 2.2e-16
#mean 13.457

# French
groupHcm<-subset(painters$Composition, (painters$School == "H"))
groupH_comp=sample(groupHcm,1000,replace = TRUE)
t.test(groupH_comp, alternative = "greater", mu = 10)
#yes 
#p-value < 2.2e-16
#mean 14.021

groupHe<-subset(painters$Expression, (painters$School == "H"))
groupH_exp=sample(groupHe,1000,replace = TRUE)
t.test(groupH_exp, alternative = "greater", mu = 10)
#yes
#p-value < 2.2e-16
#mean 12.04
