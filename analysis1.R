


pacman::p_load(dplyr,purrr,tidyr,tibble,ggplot2,gt,patchwork,ggh4x,knitr,lme4)
data <- read.csv("processed_energy_study_data.csv")

colnames(data)
head(data,1)
glimpse(data)



data %>%
  summarise(
    avg_age = mean(2021 - birth_year, na.rm = TRUE),
    gender_distribution = table(gender)
  )

avg_age <- data %>%
  summarise(avg_age = mean(birth_year, na.rm = TRUE))

final_data %>%
  count(gender)



# Reliability analysis for 'els' scale
library(psych)
els_items <- final_data %>%
  select(starts_with("els")) %>%
  mutate_all(as.numeric)
alpha_results <- alpha(els_items)
print(alpha_results)





# Visualization of scenario responses
data %>%
  ggplot(aes(x = scenario, y = `01`)) +
  geom_boxplot() +
  labs(title = "Distribution of Response '01' Across Scenarios")



model <- lmer(X01 ~ scenario + (1 | responseid), data = data)




scenario_means <- data %>%
  group_by(scenario) %>%
  summarise(mean_X01 = mean(X01, na.rm = TRUE))

scenario_means %>%
  ggplot(aes(x = scenario, y = mean_X01)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Response 'X01' Across Scenarios",
       x = "Scenario",
       y = "Mean Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
