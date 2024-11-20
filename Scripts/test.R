# Load necessary libraries
pacman::p_load(dplyr, purrr, tidyr, here, tibble, brms, rstan, bayestestR, emmeans, tidybayes, modelsummary, ggplot2, gt, knitr, kableExtra, ggh4x, lme4, flextable, pander)

# Load the ggplot2 package
library(ggplot2)

# Create a sample dataset
set.seed(123)
data <- data.frame(
  value = c(rnorm(1000, mean = 0, sd = 1), rnorm(1000, mean = 1, sd = 1)),
  group = rep(c("A", "B"), each = 1000)
)

# Define thresholds
thresholds <- c(-1, 0, 1, 2)

# Create the plot
ggplot(data, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("orange", "brown")) +
  geom_vline(xintercept = thresholds, linetype = "dotted") +
  labs(title = "Underlying latent scale for Y, given X",
       fill = "X") +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-5, 6)







# Load the ggplot2 package
library(ggplot2)

# Create a sample dataset
set.seed(123)
data <- data.frame(
  value = c(rnorm(1000, mean = 0, sd = 1), rnorm(1000, mean = 1, sd = 1)),
  group = rep(c("A", "B"), each = 1000)
)

# Define thresholds
thresholds <- c(-1, 0, 1, 2)

# Create the plot
ggplot(data, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("orange", "brown")) +
  geom_vline(xintercept = thresholds, linetype = "dotted") +
  labs(title = "Underlying latent scale for Y, given X",
       fill = "X") +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  xlim(-5, 6) +
  annotate("text", x = thresholds, y = 0.4, label = c(expression(theta[1]), expression(theta[2]), expression(theta[3]), expression(theta[4])), angle = 0, vjust = -0.5,hjust=.2)
