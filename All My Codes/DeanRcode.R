dean.df <- read.csv(paste("DeansDilemma.csv",sep=","))
View(dean.df)

summary(dean.df)

library(psych)
describe(dean.df$Salary)
median(dean.df$Salary)

round(sum(dean.df$Placement_B==1)/nrow(dean.df)*100,digits = 2)

placed.df <- dean.df[ which(dean.df$Placement_B==1), ]
View (placed.df)

median(placed.df$Salary)

#me.df =xtabs(~placed.df$Gender+placed.df$Salary, data=placed.df)
#View(me.df)

mal.df <- placed.df[ which(placed.df$Gender.B==0),]
#malsal=table(mean(me.df[,26]),0)

#me2.df <- placed.df[ which(placed.df$Gender.B==1),]
#femalsal=table(mean(me2.df[,26]),1)

by(placed.df$Salary,placed.df$Gender,mean)

notplaced.df <- dean.df[ which(dean.df$Placement_B==0),]
View(notplaced.df)

p1=hist(placed.df$Percent_MBA, 
        xlim=c(50,80), ylim=c(0,150),
        col=c("grey"),
        xlab = "MBA Percentage",
        ylab = "Count",
        main = "MBA performance of placed students",
        breaks=3)


p1=hist(placed.df$Percent_MBA, 
     xlim=c(50,80), ylim=c(0,150),
     breaks=3)



p2=hist(notplaced.df$Percent_MBA, 
     xlim=c(50,80), ylim=c(0,40), 
     breaks=3
     )

par(mfrow=c(1, 2))
with(placed.df, plot(p1,cex=0.5,main="MBA Performance of Placed Students",xlab = "MBA Percentage",ylab = "Count",col=c("grey")))
with(notplaced.df, plot(p2,cex=0.5,main="MBA Performance of Not Placed Students",xlab = "MBA Percentage",ylab = "Count",col=c("grey")))


#mal.df <- placed.df[ which(placed.df$Gender.B==0),]
#femal.df <- placed.df[ which(placed.df$Gender.B==1),]

#m = boxplot(mal.df$Salary,
#            xlim=c(,80), ylim=c(0,40),
#        horizontal=TRUE)
#f = boxplot(femal.df$Salary,
#            xlim=c(50,80), ylim=c(0,40),
#            horizontal=TRUE)

#par(mfrow=c(2,1))
#with(mal.df,plot(m,cex=0.5))
#with(femal.df,plot(f,cex=0.5))

library(lattice)

bwplot(placed.df$Gender ~ placed.df$Salary, data=placed.df, xlab="Salary",main="Comparison of Salaries of Male And Female")

placedET.df <- placed.df[ which(placed.df$Entrance_Test!="None"), ]
View(placedET.df)

library(car)
scatterplotMatrix(formula = ~placedET.df$Salary + placedET.df$Percent_MBA + placedET.df$Percentile_ET, cex=0.6,
                  data=placedET.df)
