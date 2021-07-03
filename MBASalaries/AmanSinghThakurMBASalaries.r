setwd("~/Data Analytics/workingdirectory/MBASalaries")

mba.df <- read.csv(paste("MBA Starting Salaries Data.csv",sep = ""))
                   
View(mba.df)

#summary of data
summary(mba.df)

library(car)

#boxplot of all features against salary
boxplot(mba.df$salary~mba.df$sex,data=mba.df, main="MBA Salary Vs MBA Sex", 
        xlab="MBA Sex", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$gmat_tot,data=mba.df, main="MBA Salary Vs GMAT Total", 
        xlab="Total", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$gmat_qpc,data=mba.df, main="MBA Salary Vs Quantitative GMAT percentile", 
        xlab="Quantitative GMAT percentile", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$gmat_vpc,data=mba.df, main="MBA Salary Vs Verbal GMAT percentile", 
        xlab="Verbal GMAT percentile", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$gmat_tpc,data=mba.df, main="MBA Salary Vs Overall GMAT percentile", 
        xlab="Overall GMAT percentile", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$s_avg,data=mba.df, main="MBA Salary Vs Spring MBA performance", 
        xlab="Spring MBA performance", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$f_avg,data=mba.df, main="MBA Salary Vs Fall MBA performance", 
        xlab="Fall MBA performance", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$quarter,data=mba.df, main="MBA Salary Vs Quartile ranking", 
        xlab="Quartile ranking", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$quarter,data=mba.df, main="MBA Salary Vs Years of work experience", 
        xlab="Years of work experience", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$frstlang,data=mba.df, main="MBA Salary Vs First language", 
        xlab="First language", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$age,data=mba.df, main="MBA Age Vs Starting salary", 
        xlab="Age", ylab="MBA Salary")

boxplot(mba.df$salary~mba.df$satis,data=mba.df, main="MBA Salary Vs Degree of satisfaction with MBA program", 
        xlab="Degree of satisfaction with MBA program", ylab="MBA Salary")

#Corrgram to show relationship between variables
library(corrgram)
corrgram(mba.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="MBA Salaries")

#data of only placed students
placed <- mba.df[ which(mba.df$work_yrs!=0) , ] 

View(placed)

#Contingency table for placed student with Chi-Square
mytable <- xtabs(~placed$salary+placed$sex+placed$gmat_tot+placed$work_yrs+placed$s_avg, data=placed)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence

#t.test of salary with relevant features 
t.test(placed$salary+placed$s_avg)

t.test(placed$salary+placed$gmat_tot)

t.test(placed$salary+placed$s_avg)

t.test(placed$salary+placed$work_yrs)

t.test(placed$salary+placed$sex)

#lm best fit for placed students
fitstartingsalaries <- lm(placed$salary~placed$sex+placed$frstlang+placed$gmat_tot+placed$s_avg+placed$f_avg+placed$quarter)
coef(fitstartingsalaries)

#data for not placed students
notplaced <- mba.df[ which(mba.df$work_yrs==0), ] 

#Contingency table for not placed student with Chi-Square
mytable2 <- xtabs(~notplaced$salary+notplaced$sex+notplaced$gmat_tot+notplaced$work_yrs+notplaced$s_avg, data=notplaced)
ftable(mytable2) # print table 
summary(mytable2) # chi-square test of indepedence

#lm best fit for not placed students
fitstartingsalariesnotplaced <- lm(notplaced$salary~notplaced$sex+notplaced$frstlang+notplaced$gmat_tot+notplaced$s_avg+notplaced$f_avg+notplaced$quarter)
coef(fitstartingsalariesnotplaced)
