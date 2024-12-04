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


# test 



#simulate from a guassian
