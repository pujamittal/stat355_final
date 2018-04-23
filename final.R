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
#not at the .05 level but at the .1 level
#p-value = 0.08374
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

#t-Test to compare the composition scores of B to the 16th century category 

group1<-subset(painters$Composition, (painters$School == "B"))
groupBE=sample(group1,1000,replace = TRUE)
group2<-subset(painters$Composition, (painters$School == "F"))
groupF=sample(group2,1000,replace = TRUE)

t.test(groupBE,groupF)

#t-Test to compare the drawing scores of B to the 16th century category 

groupd1<-subset(painters$Drawing, (painters$School == "B" ))
groupD1<-sample(groupd1,1000,replace = TRUE)
groupd2<-subset(painters$Drawing, (painters$School == "F"))
groupD2=sample(groupd2,1000,replace = TRUE)

t.test(groupD1,groupD2)

#t-Test to compare the Colour scores of B to the 16th century category 

groupc1<-subset(painters$Colour, (painters$School == "B"))
groupC1<-sample(groupc1,1000,replace = TRUE)
groupc2<-subset(painters$Colour, (painters$School == "F"))
groupC2=sample(groupc2,1000,replace = TRUE)

t.test(groupC1,groupC2)

#t-Test to compare the Expression scores of B to the 16th century category 

groupe1<-subset(painters$Expression, (painters$School == "B"))
plotgroupE1<-sample(groupe1,1000,replace = TRUE)
groupe2<-subset(painters$Expression, (painters$School == "F"))
groupE2=sample(groupe2,1000,replace = TRUE)

t.test(groupE1,groupE2)

#t-Test to compare sum of all the scores of B to the 16th century category 

groupsum1<-subset((Composition+Drawing+Colour+Expression), (painters$School == "B"))
groupS1<-sample(groupsum1,1000,replace=TRUE)
groupsum2<-subset((Composition+Drawing+Colour+Expression),(painters$School == "F"))
groupS2<-sample(groupsum2,1000,replace = TRUE)

t.test(groupS1,groupS2)

#t-Test to compare the composition scores of C to the 17th century category 

group1<-subset(painters$Composition, (painters$School == "C"))
groupBE=sample(group1,1000,replace = TRUE)
group2<-subset(painters$Composition, (painters$School == "G"))
groupF=sample(group2,1000,replace = TRUE)

t.test(groupBE,groupF)

#t-Test to compare the drawing scores of C to the 17th century category 

groupd1<-subset(painters$Drawing, (painters$School == "C" ))
groupD1<-sample(groupd1,1000,replace = TRUE)
groupd2<-subset(painters$Drawing, (painters$School == "G"))
groupD2=sample(groupd2,1000,replace = TRUE)

t.test(groupD1,groupD2)

#t-Test to compare the Colour scores of C to the 17th century category 

groupc1<-subset(painters$Colour, (painters$School == "C"))
groupC1<-sample(groupc1,1000,replace = TRUE)
groupc2<-subset(painters$Colour, (painters$School == "G"))
groupC2=sample(groupc2,1000,replace = TRUE)

t.test(groupC1,groupC2)

#t-Test to compare the Expression scores of C to the 17th century category 

groupe1<-subset(painters$Expression, (painters$School == "C"))
plotgroupE1<-sample(groupe1,1000,replace = TRUE)
groupe2<-subset(painters$Expression, (painters$School == "G"))
groupE2=sample(groupe2,1000,replace = TRUE)

t.test(groupE1,groupE2)

#t-Test to compare sum of all the scores of C to the 17th century category 

groupsum1<-subset((Composition+Drawing+Colour+Expression), (painters$School == "C"))
groupS1<-sample(groupsum1,1000,replace=TRUE)
groupsum2<-subset((Composition+Drawing+Colour+Expression),(painters$School == "G"))
groupS2<-sample(groupsum2,1000,replace = TRUE)

t.test(groupS1,groupS2)

#Anova(used to compare the three periods A,C,D to see if the time periods have any effect on the scores)

#Sum of all the scores 
myanova<-aov((Composition+Drawing+Colour+Expression)~School , data=groupACD)
summary(myanova)

#Composition scores
myanova<-aov((Composition)~School , data=groupACD)
summary(myanova)

#Drawing scores
myanova<-aov((Drawing)~School , data=groupACD)
summary(myanova)

#Colour scores
myanova<-aov((Colour)~School , data=groupACD)
summary(myanova)

#Expression scores
myanova<-aov((Expression)~School , data=groupACD)
summary(myanova)

#Boxplots

boxplot(Composition~School , ylab = " Composition Scores" , xlab="The school")
boxplot(Drawing~School , ylab = " Drawing Scores" , xlab="The school")
boxplot(Colour~School , ylab = " Colour Scores" , xlab="The school")
boxplot(Expression~School , ylab = " Expression Scores" , xlab="The school") 
boxplot((Composition+Drawing+Colour+Expression)~School , ylab = "Sum of all the Scores" , xlab="The school")
