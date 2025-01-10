library(dplyr)
library(ggplot2)
library(pROC)
library(tidyr)
install.packages('MuMIn')
library(MuMIn)
library(lme4)
set.seed(123)

#Load data into dataframes
df <- read.csv("data/data-agg.csv",header=T)
n <- nrow(df)
dfHeads <- df[,-c(2,4)] # heads to heads
dfTails <- df[,-c(1,3)] # tails to heads
dfTails[,1] <- dfTails[,2]-dfTails[,1] # tails to tails
names(dfHeads) <- names(dfTails) <- c("y", "m", "person", "coin")
start <- rep(c("heads","tails"),c(n,n))
df <- rbind(dfHeads,dfTails)
df$person <- factor(df$person); df$coin <- factor(df$coin); df$start <- factor(start) # nolint

df_time <- read.csv("data/df-time.csv",header=T)

#Remove outliers first
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

df_time <- df_time %>%
  mutate(
    same_side = ifelse(toss_start == toss_end, 1, 0)
    )

#Create batches of 50 tosses
df_time <- df_time %>%
  group_by(person, coin, dataset, toss_start) %>%
  mutate(time = ceiling(row_number() / 50)) %>% # Assign group numbers based on tosses
  filter(n() >= 50) %>%                          # Keep groups with at least 50 tosses
  group_by(person, coin, time, dataset, toss_start) %>%
  summarise(
    y = sum(same_side),                          # Count same_tosses
    m = n()                                      # Count total tosses (should be 50)
  ) %>%
  filter(m == 50) %>%                            # Ensure each group has exactly 50 tosses
  ungroup()
  
df_time <- df_time %>%
  select(y, m, person, coin, dataset, toss_start, time)

#Add squared time as covariate
df_time$time_squared <- df_time$time^2

#Data actually used for fitting model
print("Data reduced to:")
sum(df_time$m)
sum(df_time$m)/350757

#Models fitted (control added to make it computationally feasable)
mixed_model <- glmer(cbind(y,m-y)~1+time+time_squared+(time|person)+(time_squared|person), data=df_time, family=binomial, control = lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", starttests = FALSE, kkt = FALSE)))
mixed_modelCoin <- glmer(cbind(y,m-y)~1+time+time_squared+(time|coin)+(time_squared|coin)+(time|person)+(time_squared|person), data=df_time, family=binomial, control = lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", starttests = FALSE, kkt = FALSE)))
mixed_modelStart <- glmer(cbind(y,m-y)~1+time+time_squared+toss_start+(time|person)+(time_squared|person), data=df_time, family=binomial, control = lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", starttests = FALSE, kkt = FALSE)))
mixed_modelDataset <- glmer(cbind(y,m-y)~1+time+time_squared+dataset+(time|person)+(time_squared|person), data=df_time, family=binomial, control = lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", starttests = FALSE, kkt = FALSE)))

anova(mixed_model, mixed_modelCoin, test="LRT")
anova(mixed_model, mixed_modelStart, test="LRT")
anova(mixed_model, mixed_modelDataset, test="LRT")

#Results
summary(mixed_model)
print("Model")
AIC(mixed_model)
AICc(mixed_model)
BIC(mixed_model)
print("ModelCoin")
AIC(mixed_modelCoin)
AICc(mixed_modelCoin)
BIC(mixed_modelCoin)
print("ModelStart")
AIC(mixed_modelStart)
AICc(mixed_modelStart)
BIC(mixed_modelStart)
print("ModelDataset")
AIC(mixed_modelDataset)
AICc(mixed_modelDataset)
BIC(mixed_modelDataset)

min(fixef(mixed_model)[-1])
max(fixef(mixed_model)[-1])
mean(fixef(mixed_model)[-1])
sd(fixef(mixed_model)[-1])
min(resid(mixed_model, scaled=TRUE))
max(resid(mixed_model, scaled=TRUE))

#Create plots for model checking
plot(fitted(mixed_model), resid(mixed_model, scaled=TRUE), main="Standardized residuals vs. fitted values", xlab="Fitted values", ylab="Standardized Residuals")
plot(df_time$y/df_time$m, resid(mixed_model, scaled=TRUE), main="Standardized residuals vs. success probabilities", xlab="Success probabilities", ylab="Standardized Residuals")
plot(df_time$y/df_time$m, fitted(mixed_model))

#QQ plot
qqnorm(resid(mixed_model, scaled=TRUE), main = "QQ-plot", xlab = "Quantiles", ylab = "Empirical Quantiles")
qqline(resid(mixed_model, scaled=TRUE), col = "red", lwd = 2)

#Residual series
residuals_df <- data.frame(Residuals = resid(mixed_model, scaled=TRUE), Index = seq_along(resid(mixed_model, scaled=TRUE)))
ggplot(residuals_df, aes(x = Index, y = Residuals)) +
  geom_line() +
  labs(x = "Index", y = "Residuals") +
  ggtitle("Residuals series") +
  theme(axis.title = element_text(size = 15),  
        plot.title = element_text(size = 20))

#Cooks distance
cooks_distance <- cooks.distance(mixed_model)
plot(cooks_distance, main = "Cook's Distance", ylab = "Distance", type = "h", col = "blue")
abline(h = 4/(length(cooks_distance)), col = "red", lty = 2) # rule of thumb 

threshold <- 4 / nrow(df_time)
influential_points <- which(cooks_distance > threshold)
influential_data <- df_time[influential_points, ]
print(influential_data)

#Mean prediction
pred_probs<- predict(mixed_model, type = "response")
print(mean(pred_probs))

#ROC curve
actual_classes <- ifelse(df_time$y > df_time$m/2 , 1, 0)
roc_curve <- roc(actual_classes, pred_probs)
plot(roc_curve, main = "ROC curve", col = "blue", lwd = 2)
auc(roc_curve)


#Accuracy calculation
pred_probs <- predict(mixed_model, type = "response")
pred_successes <- pred_probs * df_time$m
pred_classes <- ifelse(pred_successes > (df_time$m / 2), 1, 0)
actual_classes <- ifelse(df_time$y > (df_time$m / 2), 1, 0)
confusion_matrix <- table(Predicted = pred_classes, Actual = actual_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))

#Plotting of time effects including interval
interceptPlus <- fixef(mixed_model)[1] + sd(ranef(mixed_model)$person[1][,]) + sd(ranef(mixed_model)$person[3][,])
interceptMinus <- fixef(mixed_model)[1] - sd(ranef(mixed_model)$person[1][,]) - sd(ranef(mixed_model)$person[3][,])
timePlusOne <- fixef(mixed_model)[2] + sd(ranef(mixed_model)$person[2][,])
timeMinusOne <- fixef(mixed_model)[2] - sd(ranef(mixed_model)$person[2][,])
timePlusTwo <- fixef(mixed_model)[3] + sd(ranef(mixed_model)$person[4][,])
timeMinusTwo <- fixef(mixed_model)[3] - sd(ranef(mixed_model)$person[4][,])
x <- seq(0,50,1)
plot(x, exp(fixef(mixed_model)[1] + x * fixef(mixed_model)[2] + (x^2) * fixef(mixed_model)[3])/(1+exp(fixef(mixed_model)[1] + x * fixef(mixed_model)[2] + (x^2) * fixef(mixed_model)[3])), main="Same-side bias", xlab="Batch group of 50 coin flips", ylab="Same-side bias", type="l", ylim=c(0.49,0.52))
lines(x, exp(interceptPlus + x * timePlusOne + (x^2) * timePlusTwo)/(1+exp(interceptPlus + x * timePlusOne + (x^2) * timePlusTwo)), type="l", lty=2)
lines(x, exp(interceptMinus + x * timeMinusOne + (x^2) * timeMinusTwo)/(1+exp(interceptMinus + x * timeMinusOne + (x^2) * timeMinusTwo)), type="l", lty=2)

exp(interceptPlus)/(1+exp(interceptPlus))
exp(interceptMinus)/(1+exp(interceptMinus))
