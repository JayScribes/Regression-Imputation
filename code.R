library(tidyverse)
library(mice)

df <- data.frame(distance = 1:200,
                 gas_consumed = rnorm(200, mean = 20, sd = 1) + 0.1 * (1:200))

df$gas_consumed[sample(1:200, 50)] <- NA

# Linear regression imputation
df_lm <- df %>% 
  mice(method = "norm.predict") %>% 
  complete()

# Random forest imputation
df_rf <- df %>% 
  mice(method = "rf", m = 10) %>% 
  complete()

## Stochastic regression imputation
df_sr <- df %>% 
  mice(method = "norm.nob", m = 10) %>% 
  complete()

get_cor <- function(df) {
  cor_coef <- cor(df$distance, df$gas_consumed, use = "pairwise.complete.obs")
  cor_label <- paste0("Correlation Coefficient: ", round(cor_coef, 2))
  return(cor_label)
}

P1 <- ggplot() + geom_point(data = df, aes(x = distance, y = gas_consumed), color = "black")+
  labs(title = "Original Missing Data",
       subtitle = get_cor(df),
       x = "Distance",
       y = "Gas Consumed")
P2 <- ggplot() + geom_point(data = df_lm, aes(x = distance, y = gas_consumed), color = "red") +
  labs(title = "Linear Regression Imputed Data",
       subtitle = get_cor(df_lm),
       x = "Distance",
       y = "Gas Consumed")

P3 <- ggplot() + geom_point(data = df_sr, aes(x = distance, y = gas_consumed), color = "blue") +
  labs(title = "Stochastic Regression Imputed Data",
       subtitle = get_cor(df_sr),
       x = "Distance",
       y = "Gas Consumed")

P4 <- ggplot() + geom_point(data = df_rf, aes(x = distance, y = gas_consumed), color = "Purple") +
  labs(title = "Random Forest Imputed Data",
       subtitle = get_cor(df_sr),
       x = "Distance",
       y = "Gas Consumed")

hist_orig <- ggplot(df, aes(x = gas_consumed)) +
  geom_histogram(fill = "black", color = "white") +
  ggtitle("Original Missing Data") +
  xlim(0,70) +
  ylim(c(0, 35))

hist_lm <- ggplot(df_lm, aes(x = gas_consumed)) +
  geom_histogram(fill = "red", color = "white") +
  ggtitle("Linear Regression Imputed Data")+
  xlim(0,70) +
  ylim(c(0, 35))

hist_sr <- ggplot(df_sr, aes(x = gas_consumed)) +
  geom_histogram(fill = "blue", color = "white") +
  ggtitle("Stochastic Regression Imputed Data")+
  xlim(0, 70) +
  ylim(c(0, 35))

hist_rf <- ggplot(df_rf, aes(x = gas_consumed)) +
  geom_histogram(fill = "Purple", color = "white") +
  ggtitle("Random Forest Imputed Data")+
  xlim(0, 70) +
  ylim(c(0, 35))

combined_plot <- plot_grid(
  P1 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_orig + theme(plot.margin = margin(0, 0, 0, 10)),
  P2 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_lm + theme(plot.margin = margin(0, 0, 0, 10)),
  P3 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_sr + theme(plot.margin = margin(0, 0, 0, 10)),
  P4 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_rf + theme(plot.margin = margin(0, 0, 0, 10)),
  ncol = 2, nrow = 4
)



