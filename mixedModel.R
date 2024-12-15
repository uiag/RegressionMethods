library(dplyr)
library(ggplot2)
library(pROC)
library(tidyr)
library(MuMIn)
library(lme4)
set.seed(123)

df <- read.csv("CoinToss/analyses/data-agg.csv",header=T)
n <- nrow(df)
dfHeads <- df[,-c(2,4)] # heads to heads
dfTails <- df[,-c(1,3)] # tails to heads
dfTails[,1] <- dfTails[,2]-dfTails[,1] # tails to tails
names(dfHeads) <- names(dfTails) <- c("y", "m", "person", "coin")
start <- rep(c("heads","tails"),c(n,n))
df <- rbind(dfHeads,dfTails)
df$person <- factor(df$person); df$coin <- factor(df$coin); df$start <- factor(start) # nolint

df_long <- read.csv("CoinToss/analyses/data-long.csv",header=T)
df_time_agg <- read.csv("CoinToss/analyses/df-time-agg.csv",header=T)
df_time <- read.csv("CoinToss/analyses/df-time.csv",header=T)

#Remove outliers first
df <- df[df$person != "TianqiPeng", ]
df <- df[df$person != "JanYang", ]
df <- df[df$coin != "0.50SGD", ]
df <- df[df$coin != "0.02EUR", ]
df <- subset(df, !(person == "MagdaMatetovici" & coin == "1CAD"))
df <- subset(df, !(person == "XiaochangZhao" & coin == "0.50SGD"))
df <- subset(df, !(person == "FranziskaAssion" & coin == "1EUR"))
df <- subset(df, !(person == "EJ" & coin == "50CZK"))
df <- subset(df, !(person == "XiaoyiLin" & coin == "0.50EUR"))
df <- subset(df, !(person == "JasonNak" & coin == "0.50EUR"))

df_long <- df_long[df_long$person != "TianqiPeng", ]
df_long <- df_long[df_long$person != "JanYang", ]
df_long <- df_long[df_long$coin != "0.50SGD", ]
df_long <- df_long[df_long$coin != "0.02EUR", ]
df_long <- subset(df_long, !(person == "MagdaMatetovici" & coin == "1CAD"))
df_long <- subset(df_long, !(person == "XiaochangZhao" & coin == "0.50SGD"))
df_long <- subset(df_long, !(person == "FranziskaAssion" & coin == "1EUR"))
df_long <- subset(df_long, !(person == "EJ" & coin == "50CZK"))
df_long <- subset(df_long, !(person == "XiaoyiLin" & coin == "0.50EUR"))
df_long <- subset(df_long, !(person == "JasonNak" & coin == "0.50EUR"))

df_time <- df_time[df_time$person != "TianqiPeng", ]
df_time <- df_time[df_time$person != "JanYang", ]
df_time <- df_time[df_time$coin != "0.50SGD", ]
df_time <- df_time[df_time$coin != "0.02EUR", ]
df_time <- subset(df_time, !(person == "MagdaMatetovici" & coin == "1CAD"))
df_time <- subset(df_time, !(person == "XiaochangZhao" & coin == "0.50SGD"))
df_time <- subset(df_time, !(person == "FranziskaAssion" & coin == "1EUR"))
df_time <- subset(df_time, !(person == "EJ" & coin == "50CZK"))
df_time <- subset(df_time, !(person == "XiaoyiLin" & coin == "0.50EUR"))
df_time <- subset(df_time, !(person == "JasonNak" & coin == "0.50EUR"))

df_time_agg <- df_time_agg[df_time_agg$person != "TianqiPeng", ]
df_time_agg <- df_time_agg[df_time_agg$person != "JanYang", ]
df_time_agg <- df_time_agg[df_time_agg$coin != "0.50SGD", ]
df_time_agg <- df_time_agg[df_time_agg$coin != "0.02EUR", ]
df_time_agg <- subset(df_time_agg, !(person == "MagdaMatetovici" & coin == "1CAD"))
df_time_agg <- subset(df_time_agg, !(person == "XiaochangZhao" & coin == "0.50SGD"))
df_time_agg <- subset(df_time_agg, !(person == "FranziskaAssion" & coin == "1EUR"))
df_time_agg <- subset(df_time_agg, !(person == "EJ" & coin == "50CZK"))
df_time_agg <- subset(df_time_agg, !(person == "XiaoyiLin" & coin == "0.50EUR"))
df_time_agg <- subset(df_time_agg, !(person == "JasonNak" & coin == "0.50EUR"))

sampled_data <- df_time_agg %>%
  group_by(person, coin) %>%
  # Calculate the cumulative number of tosses
  mutate(cumulative_tosses = cumsum(to - from + 1)) %>%
  # Identify rows within the first 2000 tosses
  filter(cumulative_tosses <= 2000 | lag(cumulative_tosses, default = 0) < 2000) %>%
  mutate(tails_tails = N_start_tails_up - tails_heads) %>%
  ungroup()

sampled_data_model <- sampled_data %>%
  mutate(y = heads_heads + tails_tails) %>%
  rename(m=N) %>%
  select(y,m,person,coin) %>%
  group_by(person, coin) %>%
  summarize(m=sum(m), y=sum(y))

df_time_agg$avg = df_time_agg$same_side / df_time_agg$N
df_time_agg <- df_time_agg %>%
  group_by(person, coin) %>%
  mutate(total_tosses = cumsum(to - from + 1)) %>%
  ungroup() %>%
  mutate(hundred_group = paste0(
    "hundred_", floor((total_tosses - 1) / 100) + 1
  ))

#MIXED MODEL
mixedIntercept <- lmer(avg~1 + (1|hundred_group), data=df_time_agg)
mixedTotal <- lmer(avg~1 + person + coin + (1|hundred_group), data=df_time_agg)
mixedTotalNested <- lmer(avg~1 + person/coin + (1|hundred_group), data=df_time_agg)
mixedPerson <- lmer(avg~1 + person + (1|hundred_group), data=df_time_agg)

anova(mixedIntercept, mixedPerson, mixedTotal, mixedTotalNested)

print("AIC:")
AIC(mixedIntercept)
AIC(mixedPerson)
AIC(mixedTotal)
AIC(mixedTotalNested)

print("AICc:")
AICc(mixedIntercept)
AICc(mixedPerson)
AICc(mixedTotal)
AICc(mixedTotalNested)

print("BIC")
BIC(mixedIntercept)
BIC(mixedPerson)
BIC(mixedTotal)
BIC(mixedTotalNested)

mixed_model <- lmer(avg~(1|hundred_group), data=df_time_agg)
summary(mixed_model)

plot(fitted(mixed_model), resid(mixed_model))
plot(df_time_agg$same_side/df_time_agg$N, resid(mixed_model))
plot(df_time_agg$same_side/df_time_agg$N, fitted(mixed_model))

qqnorm(resid(mixed_model), main = "QQ-plot", xlab = "Quantiles", ylab = "Quantiles empiriques")
qqline(resid(mixed_model), col = "red", lwd = 2)

residuals_df <- data.frame(Residuals = resid(mixed_model), Index = seq_along(resid(mixed_model)))
ggplot(residuals_df, aes(x = Index, y = Residuals)) +
  geom_line() +
  labs(x = "Index", y = "Residuals") +
  ggtitle("Residuals series") +
  theme(axis.title = element_text(size = 15),  
        plot.title = element_text(size = 20))

cooks_distance <- cooks.distance(mixed_model)
plot(cooks_distance, main = "Cook's Distance", ylab = "Distance", type = "h", col = "blue")
abline(h = 4/(length(cooks_distance)), col = "red", lty = 2) # rule of thumb 

pred_probs<- predict(mixed_model, type = "response")
print(mean(pred_probs))

actual_classes <- ifelse(df_time_agg$same_side > (df_time_agg$N / 2), 1, 0)
roc_curve <- roc(actual_classes, pred_probs)
plot(roc_curve, main = "Courbe ROC", col = "blue", lwd = 2)
auc(roc_curve)


# Prédire les probabilités de succès avec le modèle (pour chaque observation)
pred_probs <- predict(mixed_model, type = "response")

# Calculer les succès attendus (probabilité * nombre d'essais)
# Ici, m est le nombre total d'essais pour chaque observation
pred_successes <- pred_probs * df_time_agg$N

# Convertir en classes binaires : succès ou échec
# Si le nombre de succès observé est supérieur à la moitié du nombre total d'essais, on peut dire que c'est un succès global
pred_classes <- ifelse(pred_successes > (df_time_agg$N / 2), 1, 0)

# Convertir les valeurs réelles en classes binaires
# Si y (le nombre de succès observé) est supérieur à la moitié du nombre total d'essais, c'est un succès global
actual_classes <- ifelse(df_time_agg$same_side > (df_time_agg$N / 2), 1, 0)

# Créer la confusion matrix en comparant les classes binaires observées et prédites
confusion_matrix <- table(Predicted = pred_classes, Actual = actual_classes)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))

