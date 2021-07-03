store.df <- read.csv(paste("Store24.csv",sep=","))
View(store.df)

#newdata.df <- store.df[order(-store.df$Profit),]

newdata.df <- store.df[order(store.df$Profit),]

answer.df <- newdata.df[1:10,c(1,3,4,5)]

answer2.df <- answer.df[order(lol.df$Profit,decreasing = TRUE),]

View(answer2.df)

library(car)

scatterplot(store.df$Profit~store.df$MTenure, xlab = "MTenure", ylab = "Profit")

scatterplot(store.df$Profit~store.df$CTenure, xlab = "CTenure", ylab = "Profit")

lala <- cor(store.df)
pairs(lala)

cor(store.df$Profit,store.df$MTenure)

cor(store.df$Profit,store.df$CTenure)

library("corrgram")

library(corrgram)
corrgram(store.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Store")

chisq.test(store.df$Profit+store.df$MTenure)

chisq.test(store.df$Profit+store.df$CTenure)

library(psych)

summary(store.df)

mean(store.df$Profit)

mean(store.df$MTenure)

mean(store.df$CTenure)

#{MTenure, CTenure Comp, Pop, PedCount, Res, Hours24, Visibility}
fit <- lm(store.df$Profit~store.df$MTenure+store.df$CTenure+store.df$Comp+store.df$Pop+store.df$PedCount+store.df$Res+store.df$Hours24+store.df$Visibility,data = store.df)
coef(fit)

fit <- lm(store.df$Profit~store.df$CTenure, data = store.df)
fit

View(store.df)

chisq.test(store.df$Profit)
chisq.test(store.df$ServQual)

#fit <- lm(store.df$Profit, data = store.df)
#fit
