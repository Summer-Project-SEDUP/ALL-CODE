# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("D:/Downloads/out/fulloutv1/all_burstiness_metrics.csv")

# 1. Correlation Analysis
cor_burstiness = cor(data$Number_of_Events, data$Burstiness)
cor_alternative_burstiness = cor(data$Number_of_Events, data$Alternative_Burstiness)

cat("Correlation between Number of Events and Burstiness:", cor_burstiness, "\n")
cat("Correlation between Number of Events and Alternative Burstiness:", cor_alternative_burstiness, "\n")

# 2. Scatter Plots
ggplot(data, aes(x = Number_of_Events)) +
  geom_point(aes(y = Burstiness, color = 'Burstiness Points'), size = 2, alpha = 0.6) +
  geom_point(aes(y = Alternative_Burstiness, color = 'Alternative Burstiness Points'), size = 2, alpha = 0.6) +
  geom_smooth(aes(y = Burstiness, color = 'Burstiness Line'), method = 'lm', formula = y ~ poly(x, 2), se = FALSE, size = 1.2) +
  geom_smooth(aes(y = Alternative_Burstiness, color = 'Alternative Burstiness Line'), method = 'lm', formula = y ~ poly(x, 2), se = FALSE, size = 1.2) +
  labs(title = "Comparison of Burstiness Measures vs Number of Events",
       x = "Number of Events",
       y = "Burstiness Measure",
       color = "Burstiness Measure") +
  theme_minimal() +
  scale_color_manual(values = c(
    'Burstiness Points' = 'lightblue', 
    'Alternative Burstiness Points' = 'pink',
    'Burstiness Line' = 'darkblue',
    'Alternative Burstiness Line' = 'darkred'
  )) +
  guides(color = guide_legend(override.aes = list(size = 5)))



# 3. Residual Analysis
lm_burstiness <- lm(Burstiness ~ Number_of_Events, data = data)
lm_alternative_burstiness <- lm(Alternative_Burstiness ~ Number_of_Events, data = data)

par(mfrow = c(1, 2))
plot(lm_burstiness$residuals, main = "Residuals for Burstiness", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

plot(lm_alternative_burstiness$residuals, main = "Residuals for Alternative Burstiness", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# 4. Difference Analysis
data <- data %>%
  mutate(Difference = Burstiness - Alternative_Burstiness)

ggplot(data, aes(x = Number_of_Events, y = Difference)) +
  geom_point(color = 'black') +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 3), se = FALSE, color = 'blue') +
  labs(title = "Difference between Burstiness Measures vs Number of Events",
       x = "Number of Events",
       y = "Difference (Burstiness - Alternative Burstiness)") +
  theme_minimal()




