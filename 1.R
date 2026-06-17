df <- read.csv("Sleep_Health_and_Lifestyle_Dataset.csv", header = TRUE)
head(df)
max(df$Physical.Activity.Level)
min(df$Physical.Activity.Level)
df$productive <- ifelse(df$Physical.Activity.Level>=50,ifelse(df$Stress.Level<=5,1,0),0)

# Fit logistic regression
model1 <- glm(productive ~ Physical.Activity.Level + Stress.Level, data = df, family = binomial)
model2 <- glm(productive ~ Physical.Activity.Level * Stress.Level, data = df, family = binomial)
model3 <- glm(productive ~ Physical.Activity.Level : Stress.Level, data = df, family = binomial)
model4 <- glm(productive ~ Physical.Activity.Level + Physical.Activity.Level : Stress.Level, data = df, family = binomial)

summary(model1)

# Predicted probability = Productivity Index (0–1)
df$ProductivityIndex <- predict(model1, type = "response")


df$scaled_index <- scales::rescale(df$ProductivityIndex, to = c(0, 100))
df$logit_index <- log(df$ProductivityIndex / (1 - df$ProductivityIndex))
str(df)

df$Gender <- as.factor(df$Gender)
df$Occupation <- as.factor(df$Occupation)

str(df)

