library(dplyr)
df <- read.csv("CoinToss/analyses/data-agg.csv",header=T)
n <- nrow(df)
dfHeads <- df[,-c(2,4)] # heads to heads
dfTails <- df[,-c(1,3)] # tails to heads
dfTails[,1] <- dfTails[,2]-dfTails[,1] # tails to tails
names(dfHeads) <- names(dfTails) <- c("y","m","person","coin")
start <- rep(c("heads","tails"),c(n,n))
df <- rbind(dfHeads,dfTails)
df$person <- factor(df$person); df$coin <- factor(df$coin); df$start <- factor(start)

percentages <- df %>%
  group_by(person, coin) %>%
  summarise(
    success_total = 100 * sum(y) / sum(m),                                     # Pourcentage total
    success_heads = 100 * sum(y[start == "heads"]) / sum(m[start == "heads"]), # Pourcentage pour heads 
    success_tails = 100 * sum(y[start == "tails"]) / sum(m[start == "tails"]),
    m = sum(m)# Pourcentage pour tails
  ) %>%
  pivot_longer(cols = starts_with("success"), names_to = "type", values_to = "percentage")

percentagesTotal <- percentages[percentages$type == 'success_total', ]

dfStart <- df
dfStart <- dfStart %>%
       group_by(start) %>%
       summarise(across(c(y,m), sum))
results <- apply(dfStart, 1, function(row){
       y <- row["m"]
       x <- row["y"]
       binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
   })
dfStart$p_values <- sapply(results, function(res) res$p.value)
dfStart$mean  <- dfStart$y / dfStart$m

dfPerson <- df %>%
       group_by(person) %>%
       summarise(across(c(y,m), sum))
results <- apply(dfPerson, 1, function(row){
       x <- row["y"]
       y <- row["m"]
       binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
   })
dfPerson$p_values <- sapply(results, function(res) res$p.value)
dfPerson$mean <- dfPerson$y / dfPerson$m

dfPersonHeads <- dfHeads %>%
  group_by(person) %>%
  summarise(across(c(y,m), sum))
results <- apply(dfPersonHeads, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfPersonHeads$p_values <- sapply(results, function(res) res$p.value)
dfPersonHeads$mean <- dfPersonHeads$y / dfPersonHeads$m

dfPersonTails <- dfTails %>%
  group_by(person) %>%
  summarise(across(c(y,m), sum))
results <- apply(dfPersonTails, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfPersonTails$p_values <- sapply(results, function(res) res$p.value)
dfPersonTails$mean <- dfPersonTails$y / dfPersonTails$m

dfCoin <- df %>%
       group_by(coin) %>%
       summarise(across(c(y,m), sum))
results <- apply(dfCoin, 1, function(row){
       x <- row["y"]
       y <- row["m"]
       binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
   })
dfCoin$p_values <- sapply(results, function(res) res$p.value)
dfCoin$mean <- dfCoin$y / dfCoin$m

dfCoinHeads <- dfHeads %>%
  group_by(coin) %>%
  summarise(across(c(y,m), sum))
results <- apply(dfCoinHeads, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfCoinHeads$p_values <- sapply(results, function(res) res$p.value)
dfCoinHeads$mean <- dfCoinHeads$y / dfCoinHeads$m

dfCoinTails <- dfTails %>%
  group_by(coin) %>%
  summarise(across(c(y,m), sum))
results <- apply(dfCoinTails, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfCoinTails$p_values <- sapply(results, function(res) res$p.value)
dfCoinTails$mean <- dfCoinTails$y / dfCoinTails$m

results <- apply(df, 1, function(row){
       x <- row["y"]
       y <- row["m"]
       binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
   })
df$p_values <- sapply(results, function(res) res$p.value)
df$mean <- df$y / df$m

results <- apply(dfHeads, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfHeads$p_values <- sapply(results, function(res) res$p.value)
dfHeads$mean <- dfHeads$y / dfHeads$m

results <- apply(dfTails, 1, function(row){
  x <- row["y"]
  y <- row["m"]
  binom.test(as.numeric(x), as.numeric(y), p=0.5, alternative = "two.sided", conf.level=0.95)
})
dfTails$p_values <- sapply(results, function(res) res$p.value)
dfTails$mean <- dfTails$y / dfTails$m

hist(df$mean)
hist(dfHeads$mean)
hist(dfTails$mean)
hist(dfPerson$mean)
hist(dfPersonHeads$mean)
hist(dfPersonTails$mean)
hist(dfCoin$mean)
hist(dfCoinHeads$mean)
hist(dfCoinTails$mean)
boxplot(df$mean, dfHeads$mean, dfTails$mean, dfPerson$mean, dfPersonHeads$mean, dfPersonTails$mean, dfCoin$mean, dfCoinHeads$mean, dfCoinTails$mean, names=c('Total', 'TotalHeads', 'TotalTails', 'Person', 'PersonHeads', 'PersonTails', 'Coin', 'CoinHeads', 'CoinTails'), main='Distribution of success probabilities')

modelTotal = lm(mean~1+person+coin, data=df)
plot(df$person, rstandard(modelTotal))
plot(df$coin, rstandard(modelTotal))
plot(fitted(modelTotal), rstandard(modelTotal))
qqnorm(rstandard(modelTotal), pch = 1, frame = FALSE)
qqline(rstandard(modelTotal), col = "steelblue", lwd = 2)
plot(modelTotal, 4)

#Remove outliers
df <- df[df$person != "TianqiPeng", ]
df <- df[df$person != "JanYang", ]
df <- df[df$coin != "0.50SGD", ]
df <- df[df$coin != "0.02EUR", ]

dfHeads <- dfHeads[dfHeads$person != "TianqiPeng", ]
dfHeads <- dfHeads[dfHeads$person != "JanYang", ]
dfHeads <- dfHeads[dfHeads$coin != "0.50SGD", ]
dfHeads <- dfHeads[dfHeads$coin != "0.01GBP", ]

dfTails <- dfTails[dfTails$person != "TianqiPeng", ]
dfTails <- dfTails[dfTails$coin != "0.02EUR", ]
dfTails <- dfTails[dfTails$coin != "0.20CHF", ]
dfTails <- dfTails[dfTails$coin != "2INR", ]

glmIntercept = glm(cbind(y,m-y)~1,family=binomial,data=df)
glmTotal = glm(cbind(y,m-y)~1+person+coin,family=binomial,data=df)
glmPerson = glm(cbind(y,m-y)~1+person,family=binomial,data=df)
glmCoin = glm(cbind(y,m-y)~1+coin,family=binomial,data=df)

anova(glmIntercept, glmPerson, glmCoin, glmTotal)

plot(df$mean, fitted(glmTotal))
abline(c(0,1), col="red")
abline(lm(df$mean ~ fitted(glmTotal)))
plot(df$mean, resid(glmTotal))
plot(df$coin, resid(glmTotal))
plot(df$person, resid(glmTotal))
plot(fitted(glmTotal), resid(glmTotal))

step(glmTotal, direction = "both", trace = 0)

glmTotalNested = glm(cbind(y,m-y)~1+coin/person,family=binomial,data=df)

anova(glmIntercept, glmPerson, glmCoin, glmTotalNested)

plot(df$mean, fitted(glmTotalNested))
abline(c(0,1), col="red")
abline(lm(df$mean ~ fitted(glmTotalNested)))
plot(df$mean, resid(glmTotalNested))
plot(df$coin, resid(glmTotalNested))
plot(df$person, resid(glmTotalNested))
plot(fitted(glmTotalNested), resid(glmTotalNested))

#step(glmTotalNested, direction = "both", trace = 0)

wt = 1/(4*df$m)
wlsIntercept = lm(formula = mean~1, data=df, weights=wt)
wlsPerson = lm(formula = mean~1+person, data=df, weights=wt)
wlsCoin = lm(formula = mean~1+coin, data=df, weights=wt)
wlsTotal = lm(formula = mean~1+person+coin, data=df, weights=wt)

anova(wlsIntercept, wlsPerson, wlsCoin, wlsTotal)

plot(df$mean, fitted(wlsTotal))
plot(df$mean, resid(wlsTotal))
plot(df$coin, resid(wlsTotal))
plot(df$person, resid(wlsTotal))
plot(fitted(wlsTotal), resid(wlsTotal))

step(wlsTotal, direction = "both", trace = 0)

wlsTotalNested = lm(formula = mean~1+coin/person, data=df, weights=wt)

anova(wlsIntercept, wlsPerson, wlsCoin, wlsTotalNested)

plot(df$mean, fitted(wlsTotalNested))
plot(df$mean, resid(wlsTotalNested))
plot(df$coin, resid(wlsTotalNested))
plot(df$person, resid(wlsTotalNested))
plot(fitted(wlsTotalNested), resid(wlsTotalNested))

#step(wlsTotalNested, direction = "both", trace = 0)
