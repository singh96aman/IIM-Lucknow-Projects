dean.df <- read.csv(paste("DeansDilemma.csv",sep=","))
View(dean.df)


placed.df <- dean.df[ which(dean.df$Placement_B==1), ]
View (placed.df)

mal.df <- placed.df[ which(placed.df$Gender.B==0),]
View(mal.df)
femal.df <- placed.df[ which(placed.df$Gender.B==1),]

lol = xtabs(~mal.df$Gender+mal.df$Salary, data = mal.df)
View(lol)

library(MASS)
attach(dean.df)
library(psych)
#describe(placed.df$Salary)

mal.df <- placed.df[ which(placed.df$Gender.B==0),]
femal.df <- placed.df[ which(placed.df$Gender.B==1),]

by(placed.df$Salary,placed.df$Gender,mean)

t.test(placed.df$Salary,placed.df$Gender.B,paired=TRUE)



############


titanic.df <- read.csv(paste("TitanicData.csv",sep=","))
View(titanic.df)

by(titanic.df$Age,titanic.df$Survived,mean)
table_survive=by(titanic.df$Age,titanic.df$Survived,mean)

survive.df <- titanic.df[which(titanic.df$Survived==1),]
die.df <- titanic.df[which(titanic.df$Survived==0),]

library(reshape)

table_survive = table(survive.df$Age)
table_survive

table_dead = table(die.df$Age)
table_dead

t.test(titanic.df$Age, titanic.df$Survived ,data=titanic.df)


