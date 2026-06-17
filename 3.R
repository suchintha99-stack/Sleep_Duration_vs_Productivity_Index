library(ggplot2)

# Example: Sleep.Duration quadratic effect
ggplot(df, aes(x = Sleep.Duration, y = logit_index)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  labs(title = "Effect of Sleep Duration on productivity Index",
       x = "Sleep Duration",
       y = "productivity Index") +
  theme_minimal()


# Quality of Sleep quadratic effect
ggplot(df, aes(x = Quality.of.Sleep, y = logit_index)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  labs(title = "Effect of Sleep Duration on productivity Index",
       x = "Quality of Sleep",
       y = "productivity Index") +
  theme_minimal()



# Gender effect
ggplot(df, aes(x = Gender, y = logit_index)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Productivity Index by Gender",
       x = "Gender",
       y = "Productivity Index") +
  theme_minimal()

# Occupation effect
ggplot(df, aes(x = Occupation, y = logit_index)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Logit Index by Occupation",
       x = "Occupation",
       y = "Logit Index") +
  theme_minimal()



df$predicted <- predict(model5)

ggplot(df, aes(x = predicted, y = logit_index)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual productivity Index",
       x = "Predicted productivity Index",
       y = "Actual productivity Index") +
  theme_minimal()


# Example interaction: Gender * Sleep Duration
ggplot(df, aes(x = Sleep.Duration, y = logit_index, color = Gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2, raw=T), se=FALSE) +
  labs(title = "Interaction: Sleep Duration by Gender",
       x = "Sleep Duration",
       y = "Logit Index") +
  theme_minimal()

# Load required library
library(ggplot2)

# Suppose your model is stored as:
lm_model <- model5
# Create a sequence of Sleep.Duration values for prediction
sleep_seq <- seq(min(df$Sleep.Duration), max(df$Sleep.Duration), length.out = 100)

# Fix the Quality.of.Sleep level (e.g., 6)
fixed_quality <- 10

# Create a prediction dataset for both genders
new_data <- expand.grid(
  Sleep.Duration = sleep_seq,
  Gender = c("Male", "Female"),
  Quality.of.Sleep = fixed_quality
)

# Get predicted values using the model
new_data$Predicted_Index <- predict(lm_model, newdata = new_data)

# Plot the predicted productivity index vs sleep duration for each gender
ggplot(new_data, aes(x = Sleep.Duration, y = Predicted_Index, color = Gender)) +
  geom_line(size = 1.2) +
  labs(
    title = paste("Predicted Productivity Index vs Sleep Duration\n(Quality of Sleep =", fixed_quality, ")"),
    x = "Sleep Duration (hours)",
    y = "Predicted Productivity Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank()
  )

