titanic.df <- read.csv(paste("TitanicData.csv",sep=","))
View(titanic.df)
nrow(titanic.df)
#aggregate(titanic.df$Survived, by=list(titanic.df$Survived=1))
#survivors.df <- filter(titanic.df,titanic.df$Survived==1)
#nrow(survivors)
#totalsurvivos
#a <- table(titanic.df)

sum(titanic.df$Survived==1)

sum(titanic.df$Survived==0)/nrow(titanic.df)*100
sum(titanic.df$Survived==1)/nrow(titanic.df)*100

mysurvive = xtabs(~titanic.df$Survived, data = titanic.df)
View(mysurvive)

firstclasspassenger = xtabs(~titanic.df$Survived+titanic.df$Pclass, data = titanic.df)
prop.table(firstclasspassenger)*100
View(firstclasspassenger)

#firstclasspassenger = xtabs(~titanic.df$Survived+titanic.df$Pclass, data = titanic.df)
#prop.table(firstclasspassenger)*100
#View(firstclasspassenger)

firstclass = sum(titanic.df$Pclass==1)
survive = sum(titanic.df$Survived==1 & titanic.df$Pclass==1)
survive/firstclass*100

sum(titanic.df$Survived==1 & titanic.df$Pclass==1 & titanic.df$Sex=="female")

survive = sum(titanic.df$Survived==1)
female = sum(titanic.df$Sex=="female" & titanic.df$Survived==1)
female/survive*100

survive = sum(titanic.df$Survived==1 & titanic.df$Sex=="female")
female = sum(titanic.df$Sex=="female")
survive/female*100

#sum(titanic.df$Survived!=1 | titanic.df$Sex!="female")/nrow(titanic.df)*100

a <- xtabs(~titanic.df$Sex+titanic.df$Survived)
chisq.test(a)




