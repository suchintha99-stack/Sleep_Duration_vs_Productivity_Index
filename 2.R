df$duration_sq <- (df$Sleep.Duration)^2
df$quality_sq <- (df$Quality.of.Sleep)^2

# Fit linear regression
model2 <- lm(logit_index ~ Gender  + Sleep.Duration + Quality.of.Sleep, data = df)
model3 <- lm(logit_index ~ Gender + Occupation + poly(Sleep.Duration,2,raw = T) + Quality.of.Sleep, data = df)
model3 <- lm(logit_index ~ Gender + Occupation + poly(Sleep.Duration,2,raw = T) + poly(Quality.of.Sleep,2,raw=T), data = df)
model3 <- lm(logit_index ~ Gender + Occupation + poly(Sleep.Duration,2,raw = T) + poly(Quality.of.Sleep,2,raw=T), data = df)
model3 <- lm(logit_index ~ Gender + Occupation + duration_sq + quality_sq, data = df)
model4 <- lm(logit_index ~ Gender  + poly(Sleep.Duration,2,raw = T) + poly(Quality.of.Sleep,2,raw=T), data = df)
model5 <- lm(logit_index ~ Gender  + poly(Sleep.Duration,2,raw = T) + Quality.of.Sleep, data = df)
model6 <- lm(logit_index ~ Gender  +Sleep.Duration+ Quality.of.Sleep, data = df)
# View summary
summary(model2)
summary(model3)
summary(model4)
summary(model5)
plot(df$Age,df$logit_index)
plot(df$Sleep.Duration,df$logit_index)
plot(df$Quality.of.Sleep,df$logit_index)
plot(df$Gender,df$logit_index)
plot(df$Occupation,df$logit_index)



# Get the design matrix
X <- model.matrix(model3)
head(X)

levels(df$Age)
