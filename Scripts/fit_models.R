
library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here,tibble,brms,bayestestR,emmeans,tidybayes,modelsummary,
               ggplot2,gt,ggh4x,lme4,flextable)

options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)
options(brms.backend="cmdstanr",mc.cores=4)







s1 <- readRDS(here::here("data/s1_processed.rds"))

s1_agg <- s1 |> 
  filter(appliance !="Total kWh") |> 
  group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
  summarise(total_kWh = sum(value),orig_kWh=sum(family), 
            pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
            n_change = sum(value!=family),
            state_p_dif=mean(state_p_dif),
            state_f_dif=mean(state_f_dif),
            n_less_avg = sum(less_avg)) |> 
  mutate(matched_goal = (pct_change == pct_goal), 
             error = pct_change - pct_goal,
            abs_error = abs(error),
            close_match = abs_error <= 0.02)




s1_agg4 <- s1_agg |> group_by(id,refClass,calc) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) 



s1_bb1 <- brm(
  mg | trials(4) ~ refClass,
  family = beta_binomial(),
  cores=4,
  iter=5000,
  data = s1_agg4,
  file=paste0(here::here("data/model_cache",'s1_bb1.rds'))
)
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -0.58      0.01    -0.61    -0.56 1.00     9576     7793
# refClassPercentage    -0.70      0.02    -0.74    -0.66 1.00     8559     7667
# refClassUSD           -1.74      0.02    -1.78    -1.69 1.00     8391     7522
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# phi     0.33      0.00     0.32     0.34 1.00     9328     7470


pp_check(s1_bb1, type = "stat_grouped", stat = "mean", group = "refClass")




s1_bb1 %>%
  gather_draws(b_Intercept, b_refClassPercentage, b_refClassUSD) %>%
  ggplot(aes(x = .value, fill = .variable)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Posterior Distributions of Fixed Effects for s1_bb1",
       x = "Coefficient Value",
       fill = "Reference Class")

# Visualize fixed effects with credible intervals
s1_bb1 %>%
  gather_draws(b_Intercept, b_refClassPercentage, b_refClassUSD) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye() +
  theme_minimal() +
  labs(title = "Posterior Estimates of Log-Odds by Reference Class (Model s1_bb1)",
       x = "Log-Odds Estimate",
       y = "Reference Class")

s1_bb1 |> emmeans(~refClass, type="response")

s1_bb1_phi <- brm(
  bf(mg | trials(4) ~ refClass,
  phi ~ refClass),
  family = beta_binomial(),
  cores=4,
  iter=5000,
  data = s1_agg4,
  file=paste0(here::here("data/model_cache",'s1_bb1_phi.rds'))
)
# Regression Coefficients:
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.57      0.01    -0.59    -0.54 1.00    13133     7947
# phi_Intercept             -1.33      0.02    -1.37    -1.29 1.00    12232     7866
# refClassPercentage        -0.78      0.02    -0.82    -0.74 1.00    10950     7407
# refClassUSD               -1.70      0.02    -1.75    -1.66 1.00     9707     8042
# phi_refClassPercentage     0.61      0.03     0.55     0.66 1.00    11295     7879
# phi_refClassUSD            0.00      0.04    -0.07     0.07 1.00     9033     7959



s1_bb1_phi <- brm(
  bf(mg | trials(4) ~ refClass,
  phi ~ 0+refClass),
  family = beta_binomial(),
  cores=4,
  iter=5000,
  data = s1_agg4,
  file=paste0(here::here("data/model_cache",'s1_bb1_phi2.rds'))
)
# Regression Coefficients:
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.57      0.01    -0.59    -0.54 1.00    13705     8430
# refClassPercentage        -0.78      0.02    -0.82    -0.74 1.00    12152     8262
# refClassUSD               -1.70      0.02    -1.75    -1.66 1.00    10437     7905
# phi_refClasskWh           -1.33      0.02    -1.37    -1.29 1.00    12832     7597
# phi_refClassPercentage    -0.73      0.02    -0.77    -0.68 1.00    13117     7790
# phi_refClassUSD           -1.33      0.03    -1.39    -1.27 1.00    12829     8129



s1_bb1_c <- brm(
  mg | trials(4) ~ refClass*calc,
  family = beta_binomial(),
  data = s1_agg4,
  file=paste0(here::here("data/model_cache",'s1_bb1_c.rds'))
)



pp_check(s1_bb1, type = "stat_grouped", group="refClass",ndraws=150)
pp_check(s1_bb1, type = "overlaid", group="refClass",ndraws=150)

plot(conditional_effects(s1_bb1, effects="refClass"),points=FALSE)

posterior_predict(s1_bb1,ndraws=100) |> array_branch(margin=1) |> 
  map_dfr(
    function(yrep_iter){
      s1_agg4 %>% ungroup() |>  mutate(pred=yrep_iter)
    },
    .id='iter'
  ) |> mutate(iter=as.numeric(iter)) |> ggplot(aes(pred,group=iter)) +
  geom_line(alpha = .05, stat = 'density', color = 'black') +
  geom_density(data = s1_agg4,
               aes(mg,col=refClass),
               inherit.aes = FALSE,
               size = 0.5) + 
  facet_wrap(~refClass)

posterior_predict(s1_bb1,ndraws=100) |> array_branch(margin=1) |> 
  map_dfr(
    function(yrep_iter){
      s1_agg4 %>% ungroup() |>  mutate(pred=yrep_iter)
    },
    .id='iter'
  ) |> mutate(iter=as.numeric(iter)) |> 
  ggplot(aes(pred,group=iter)) +
  geom_line(alpha = .70, stat='density',color = 'blue') +
  geom_bar(data = s1_agg4,
               aes(mg,fill=refClass),
               inherit.aes = FALSE,
               size = 0.5) + 
  facet_wrap(~refClass)


posterior_predict(s1_bb1,ndraws=200) |> array_branch(margin=1) |> 
  map_dfr(
    function(yrep_iter){
      s1_agg4 %>% ungroup() |>  mutate(pred=yrep_iter)
    },
    .id='iter'
  ) |> mutate(iter=as.numeric(iter)) |> 
  ggplot(aes(pred, group=iter)) +
  # Scale density to match the count scale
  geom_line(stat='density', aes(y=..count..), 
            alpha=0.7, color='blue') +
  geom_bar(data=s1_agg4, aes(mg, fill=refClass),
           inherit.aes=FALSE) +
  facet_wrap(~refClass)





s2_long <- readRDS(here::here("data/s2_processed.rds"))


s2_agg <- s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, refClass, state, pct_goal, pct, rounded, block, plan, calc, edu) |> 
  summarise(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(matched_goal = (pct_change == pct),
          error = pct_change - pct,
          abs_error = abs(error),
         close_match = abs(pct_change - pct) <= 0.02, 
         close_match2 = abs(pct_change - pct) <= 0.05)




s2_agg4 <- s2_agg |> group_by(id,refClass,calc) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) 


s2_agg2 <- s2_agg |> group_by(id,refClass,rounded,pct_goal,pct,calc) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) 




s2_bb1 <- brm(
  mg | trials(4) ~ refClass,
  family = beta_binomial(link = "identity", link_phi = "identity"),
  data = s2_agg4,
  cores=4,
  iter=5000,
  file=paste0(here::here("data/model_cache",'s2_bb1.rds'))
)
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.41      0.00     0.40     0.42 1.00     5186     5278
# refClassPercentage    -0.12      0.00    -0.13    -0.11 1.00     5379     4249
# refClassUSD           -0.26      0.00    -0.27    -0.25 1.00     5323     5032
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# phi     0.42      0.01     0.41     0.43 1.00     5391     4484




s2_bb2_r <- brm(
  mg | trials(2) ~ refClass*rounded,
  family = beta_binomial(),
  data = s2_agg2,
  cores=4,
  iter=5000,
  file=paste0(here::here("data/model_cache",'s2_bb2_r.rds'))
)

# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            -0.44      0.01    -0.46    -0.42 1.00     6274     6840
# refClassPercentage                   -1.12      0.02    -1.15    -1.08 1.00     5516     6345
# refClassUSD                          -1.09      0.02    -1.13    -1.06 1.00     5890     5767
# roundedRounded                        0.27      0.02     0.24     0.30 1.00     5178     6802
# refClassPercentage:roundedRounded     0.72      0.02     0.68     0.77 1.00     5055     5781
# refClassUSD:roundedRounded           -0.36      0.03    -0.42    -0.31 1.00     5459     6323
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# phi     0.22      0.00     0.22     0.22 1.00     9491     6929


s2_bb2_r_g <- brm(
  mg | trials(2) ~ refClass*rounded*pct_goal,
  family = beta_binomial(),
  data = s2_agg2,
  cores=4,
  iter=5000,
  file=paste0(here::here("data/model_cache",'s2_bb2_r_g.rds'))
)
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                        -0.39      0.02    -0.42    -0.35 1.00     3631     6394
# refClassPercentage                               -1.83      0.03    -1.88    -1.77 1.00     3786     5475
# refClassUSD                                      -1.42      0.03    -1.48    -1.36 1.00     3917     5985
# roundedRounded                                    0.18      0.02     0.14     0.22 1.00     3756     5905
# pct_goal15%                                      -0.09      0.02    -0.14    -0.05 1.00     3391     5696
# refClassPercentage:roundedRounded                 1.67      0.04     1.59     1.74 1.00     3971     5681
# refClassUSD:roundedRounded                        0.14      0.04     0.06     0.21 1.00     3978     6229
# refClassPercentage:pct_goal15%                    1.36      0.04     1.29     1.43 1.00     3578     5746
# refClassUSD:pct_goal15%                           0.62      0.04     0.54     0.69 1.00     4257     6123
# roundedRounded:pct_goal15%                        0.18      0.03     0.12     0.24 1.00     3734     5616
# refClassPercentage:roundedRounded:pct_goal15%    -1.80      0.05    -1.89    -1.70 1.00     3660     5888
# refClassUSD:roundedRounded:pct_goal15%           -0.98      0.06    -1.09    -0.87 1.00     3993     6232
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# phi     0.22      0.00     0.22     0.23 1.00     9461     7339


plot(conditional_effects(s2_bb2_r_g, 
                         effects = "rounded:pct_goal", 
                         conditions=make_conditions(s2_bb2_r_g,"refClass" )),
     points=FALSE,plot=TRUE)


patchwork::wrap_plots(plot(conditional_effects(s2_bb2_r_g),points=FALSE,plot=FALSE))

marginaleffects::plot_predictions(s2_bb2_r_g, condition = list(
  "rounded","pct_goal",
  "refClass" = c("kWh","USD","Percentage")
)
)

pp_check(s2_bb2_r_g)




posterior_predict(s2_bb2_r_g,ndraws=200) |> array_branch(margin=1) |> 
  map_dfr(
    function(yrep_iter){
      s2_agg2 %>% ungroup() |>  mutate(pred=yrep_iter)
    },
    .id='iter'
  ) |> mutate(iter=as.numeric(iter)) |> 
  ggplot(aes(pred,group=iter)) +
  geom_line(stat='density', aes(y=..count..), 
            alpha=0.7, color='blue') +
  geom_bar(data = s2_agg2,
           aes(mg,fill=refClass),
           inherit.aes = FALSE,
           size = 0.5) + 
  facet_wrap(rounded~refClass)



m4_n <- brm(
  matched_goal ~ refClass + n_change + (state|id),
  data = s1_agg,
  family = bernoulli()
) 
m4_n |> emmeans(~refClass, type="response")
#-------

m <- brm(as.numeric(matched_goal) | trials(1) ~ refClass + (1|id), 
  data = s1_agg, 
  family = zero_inflated_binomial(), control = list(adapt_delta = .94, max_treedepth = 13))

m |> emmeans(~refClass, type="response")


s1_agg_binom <- s1_agg |> 
    group_by(id, refClass) |> 
    summarise(successes = sum(matched_goal), trials = n())

m5_zib <- brm(
    successes | trials(trials) ~ refClass + (1|id),
    data = s1_agg_binom,
    family = zero_inflated_binomial(),
    iter=5000,
    control = list(adapt_delta = .94, max_treedepth = 14),
    file = paste0(here::here("data/model_cache",'s1_mg1_zib.rds'))
)
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              1.08      1.62    -2.31     3.88 1.03      156      544
# refClassPercentage    -3.30      1.48    -6.41    -0.55 1.00      930     1687
# refClassUSD           -6.72      1.70   -10.58    -3.81 1.00     1218     1615


m5_zib |> emmeans(~refClass, type="response")
#  refClass   prob lower.HPD upper.HPD
#  kWh        0.78     0.136      0.99
#  Percentage 0.11     0.000      0.55
#  USD        0.00     0.000      0.06

pp_check(m5_zib, type = "stat_grouped", stat = "mean", group = "refClass")





m5_zib_c <- brm(
  successes | trials(trials) ~ refClass + calc + (1|id),
  data = s1_agg |> 
    group_by(id, refClass,calc) |> 
    summarise(successes = sum(matched_goal), trials = n()),
  family = zero_inflated_binomial(),
  iter=5000,
  control = list(adapt_delta = .94, max_treedepth = 14),
  file = paste0(here::here("data/model_cache",'s1_mg1_zib_c.rds'))
)
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -4.36      2.02    -8.70    -0.83 1.00      971     2159
# refClassPercentage    -3.65      1.43    -6.71    -1.02 1.00     3684     5180
# refClassUSD           -6.91      1.70   -10.68    -3.97 1.00     3631     5365
# calcUsedCalculator     6.43      1.78     3.33    10.37 1.00     4343     5670
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# zi     0.33      0.12     0.06     0.53 1.00      483      697

m5_zib_c |> emmeans(~refClass, type="response")
# refClass    prob lower.HPD upper.HPD
# kWh        0.265   0.00046      0.75
# Percentage 0.009   0.00000      0.10
# USD        0.000   0.00000      0.01



m4_success <- brm(
  ever_succeed ~ refClass + (1|id),
  family = bernoulli(),
  data = s1_agg |> 
    group_by(id, refClass) |> 
    summarize(ever_succeed = max(matched_goal))
)

# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -0.60      0.99    -2.94     0.99 1.01     1808     1518
# refClassPercentage    -2.19      1.92    -7.34     0.09 1.02      297      432
# refClassUSD           -5.86      3.94   -16.86    -1.60 1.03      150      311

m4_success |> emmeans(~refClass, type="response")
#  refClass   response lower.HPD upper.HPD
#  kWh            0.39    0.0077      0.66
#  Percentage     0.09    0.0000      0.32
#  USD            0.00    0.0000      0.10


m4_f <- brm(
  matched_goal ~ refClass,
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg1_f.rds')),
  family = bernoulli()
) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -0.55      0.12    -0.79    -0.33 1.00     3223     2930
# refClassPercentage    -0.79      0.18    -1.14    -0.44 1.00     2425     2922
# refClassUSD           -1.73      0.21    -2.15    -1.33 1.00     2399     2627

m4_f |> emmeans(~refClass, type="response")
#  refClass   response lower.HPD upper.HPD
#  kWh            0.37     0.311      0.42
#  Percentage     0.21     0.167      0.25
#  USD            0.09     0.066      0.12

# Point estimate displayed: median 
# Results are back-transformed from the logit scale 
# HPD interval probability: 0.95 

#-------
  
m4_s_calc <- brm(
  matched_goal ~ refClass + calc + (1|id) + (1|state),
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg_s_calc.rds')),
  family = bernoulli()
) 

m4_s_calc |> emmeans(~refClass, type="response",
  at=list(calc=c("No Calculator","Used Calculator")),re_formula = NA)
#  refClass   response lower.HPD upper.HPD
#  kWh          0.0138  0.000043     0.090
#  Percentage   0.0003  0.000000     0.004
#  USD          0.0000  0.000000     0.000


#---

m4_s_calc2 <- brm(
  matched_goal ~ refClass*calc + (1|id) + (1|state),
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg_s_calc2.rds')),
  control = list(adapt_delta = .94, max_treedepth = 13),
  family = bernoulli()
) 
# Regression Coefficients:
#                                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                -8.08      2.65   -13.81    -3.37 1.00      994     1724
# refClassPercentage                       -2.11      3.82    -9.80     5.28 1.01      894     1378
# refClassUSD                             -40.70     40.28  -162.17    -5.59 1.00      722      548
# calcUsedCalculator                        7.24      2.73     2.41    13.07 1.01      935     1468
# refClassPercentage:calcUsedCalculator    -1.96      4.13   -10.15     5.98 1.01      844     1303
# refClassUSD:calcUsedCalculator           33.33     40.14    -2.23   155.22 1.00      726      545

m4_s_calc2 |> emmeans(~refClass*calc, type="response")

pp_check(m4_s_calc2, type = "stat_grouped", stat = "mean", group = "refClass")





#-----

m4 <- brm(
  matched_goal ~ refClass + (1|id),
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg1.rds')),
  family = bernoulli()
) 
# Formula: matched_goal ~ refClass + (1 | id) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -2.35      0.96    -4.32    -0.58 1.00      627     1130
# refClassPercentage    -3.18      1.40    -6.23    -0.60 1.01      516     1112
# refClassUSD           -7.18      1.85   -11.33    -3.96 1.01      470      953

pp_check(m4, type = "stat_grouped", stat = "mean", group = "refClass")


m4 |> emmeans(~refClass, type="response")

m4 %>%
  gather_draws(b_Intercept,b_refClassPercentage, b_refClassUSD) %>%
  ggplot(aes(x = .value, fill = .variable)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Posterior Distributions of Fixed Effects for m4",
       x = "Coefficient Value",
       fill = "Reference Class")

# table
m4 %>%
  gather_draws(b_refClassPercentage, b_refClassUSD) %>%
  describe_posterior() %>%
  kable(booktabs = TRUE)



s1_agg |> group_by(refClass) |>
  summarise(n=n(),sum=sum(matched_goal), pct=sum/n)

s1_agg |> group_by(id,refClass) |>
  summarise(n=n(),sum=sum(matched_goal), pct=sum/n) |> 
  group_by(refClass) |> summarise(mean_pct=median(pct),median_pct=median(pct),sd_pct=sd(pct),nonzero=sum(pct>0),n=n())

s1_agg |> group_by(id,refClass) |>
      summarise(n=n(),sum=sum(matched_goal), pct=sum/n) |> arrange(-pct)


# find cases of na

s1_agg |> ggplot(aes(x=matched_goal,fill=refClass)) + geom_bar(position="dodge")

s1_agg |> group_by(id,refClass) |>
  summarise(n=n(),sum=sum(matched_goal), pct=sum/n) |> 
  ggplot(aes(x=sum,fill=refClass)) + geom_bar(position="dodge")


# table

s1_bb1 |> 
  gather_draws(b_refClassPercentage, b_refClassUSD) |> 
  describe_posterior() |> 
  kable(booktabs = TRUE)



m4_refined <- brm(
  matched_goal ~ refClass + (1 | id),
  data = s1_agg,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 2), class = "b"),
    prior(student_t(3, 0, 2.5), class = "sd")  # Weakly informative prior for random effects
  ),
  control = list(adapt_delta = 0.99),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 123
)

# tables of m4_refined
m4_refined |> 
  gather_draws(b_refClassPercentage, b_refClassUSD) |> 
  describe_posterior() |> 
  kable(booktabs = TRUE)
# parameter tables
m4_refined |> 
  describe_posterior() |>
  kable(booktabs = TRUE)


priors <- c(
  prior(normal(0, 1), class = "b"),            # Fixed effects priors
  prior(student_t(3, 0, 2.5), class = "sd"),   # Random effects priors
  prior(normal(0, 1), class = "Intercept")     # Intercept prior
)

# Fit the model
m4_informative <- brm(
  matched_goal ~ refClass + (1 | id),
  data = s1_agg,
  family = bernoulli(),
  prior = priors,
  control = list(adapt_delta = 0.99),
  iter = 6000, warmup = 2000, chains = 4, cores = 4,
  seed = 123
)



s1_agg <- s1_agg %>%
  mutate(within_5_percent = abs_error <= 0.05)

# Fit the logistic regression model
m4_within5 <- brm(
  within_5_percent ~ refClass + (1 | id),
  data = s1_agg,
  family = bernoulli(),
  prior = priors,
  control = list(adapt_delta = 0.99),
  iter = 6000, warmup = 2000, chains = 4, cores = 4,
  seed = 123
)







# Fit the zero-inflated binomial model 
m4_zi_bin <- brm( 
  successes | trials(trials) ~ refClass + (1 | id), 
  data = s1_agg_zi, 
  family = zero_inflated_binomial(), 
  control = list(adapt_delta = 0.99), 
  prior = priors_zi, 
  iter = 6000, warmup = 2000, chains = 4, cores = 4, 
  seed = 123 
)                                                                                                 

#  summary(m4_zi_bin)
# Family: zero_inflated_binomial 
# Links: mu = logit; zi = identity 
# Formula: successes | trials(trials) ~ refClass + (1 | id) 
# Data: s1_agg_zi (Number of observations: 252) 
# Draws: 4 chains, each with iter = 6000; warmup = 2000; thin = 1;
# total post-warmup draws = 16000
# 
# Multilevel Hyperparameters:
#   ~id (Number of levels: 252) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     4.25      0.94     2.61     6.31 1.00      895     2837
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.52      1.23    -2.14     2.56 1.00      692     1481
# refClassPercentage    -1.64      1.03    -3.68     0.39 1.00     3434     5809
# refClassUSD           -4.14      1.05    -6.22    -2.09 1.00     5245     7323
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# zi     0.44      0.12     0.15     0.63 1.00      645     1133
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).



s1_agg |> 
  # plot the distribution of the binomial matched_goal variable
  ggplot(aes(x = matched_goal)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Matched Goal Variable",
       x = "Number of Matched Goals",
       y = "Frequency")

# Error in `geom_histogram()`:
#   ! Problem while computing stat.
# ℹ Error occurred in the 1st layer.
# Caused by error in `setup_params()`:
#   ! `stat_bin()` requires a continuous x aesthetic.
# ✖ the x aesthetic is discrete.
# ℹ Perhaps you want `stat="count"`?

s1_agg |> 
  ggplot(aes(x = matched_goal)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Matched Goal Variable",
       x = "Number of Matched Goals",
       y = "Frequency") 

# plot that displays individual differences too (id), via points along with the effect of refClass
s1_agg |> 
  ggplot(aes(x = id, y = matched_goal)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Matched Goal by Reference Class and Individual",
       x = "Reference Class",
       y = "Matched Goal")




model_abs_error <- brm(
  formula = abs_error ~ refClass + (1 | id),
  data = s1_agg,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "b"),       # Prior for fixed effects
    prior(normal(0, 5), class = "Intercept") # Prior for intercept
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 123
)
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.12      0.02     0.08     0.15 1.00     2169     4136
# refClassPercentage    -0.01      0.03    -0.06     0.04 1.00     2278     4454
# refClassUSD            0.04      0.02    -0.01     0.09 1.00     2061     4096

model_matched_goal <- brm(
  formula = matched_goal ~ refClass + (1 | id),
  data = s1_agg,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 2), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 123
)
# Formula: matched_goal ~ refClass + (1 | id) 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -2.87      0.82    -4.57    -1.38 1.00     3520     5608
# refClassPercentage    -1.52      0.97    -3.39     0.41 1.00     3777     5883
# refClassUSD           -4.19      1.02    -6.24    -2.23 1.00     4787     7207



s1_agg <- s1_agg |> mutate(close_match = abs_error < 0.02)

m4c <- brm(
  close_match ~ refClass + (1|id),
  data = s1_agg,
  cores=4,
  family = bernoulli()
)



# prior = c(prior(normal(30,200),lb=0,class= Intercept))
# modelName <- "combo_testVxBand_RF_5K.rds"
# combo_vx <- brm(vx ~ condit * fb * bandOrder * bandInt + (1 + bandInt|id),
#                 data=testAll,file=paste0(here::here("data/model_cache",modelName)),
#                 iter=5000,chains=4,silent=0, prior=prior, 
#                 control=list(adapt_delta=0.94, max_treedepth=11))
# bayestestR::describe_posterior(combo_vx)

#  posterior_summary(bmtd,variable=r_bandInt_params)


# get_coef_details <- function(model, term_name) {
#   term_details <- broom.mixed::tidy(model) %>%
#     filter(term == term_name) %>%
#     select(estimate, conf.low, conf.high) %>%
#     mutate(across(where(is.numeric), ~round(.x, 2)))
#   
#   pd_results <- bayestestR::p_direction(model, effects = "fixed", parameters = term_name)
#   # Directly extract the pd value for the specified term
#   pd_value <- ifelse(!is.null(pd_results$pd), pd_results$pd[1], NA_real_)
#   
#   term_details$pd <- paste0(round(pd_value * 100, 2),"%")  # Ensure it's a percentage and rounded
#   return(term_details)
# }

# bayestestR::p_direction(e1_vxBMM)[, "conditVaried"]


# condEffects <- function(m){
#   m |> ggplot(aes(x = bandInt, y = .value, color = condit, fill = condit)) + 
#     stat_dist_pointinterval() + stat_halfeye(alpha=.2) +
#     stat_lineribbon(alpha = .25, size = 1, .width = c(.95)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
#     ylab("Predicted X Velocity") + xlab("Band")
# }



# GetModelStats <- function(model, type="brms") {
#   # Get current model stats for brms model
#   if (type == "brms") {
#     m1 <- as.data.frame(describe_posterior(model, centrality = "Mean"))
#     m2 <- fixef(model)
#     df <- cbind(m1[, c(1,2)], m1[, c(4,5, 6)])
#     #df <- cbind(m1[, c(1,2)], m2[, 2], m1[, c(4,5, 6, 11, 12)])
#     # colnames(df) <- c("Term", "\\(\\beta_{Median}\\)", "SD",
#     #                   "95% CrI \nLower", "95% CrI \nUpper", "pd", "\\(\\widehat R\\)", "ESS")
#     
#     colnames(df) <- c("Term", "Estimate",
#                       "95% CrI Lower", "95% CrI Upper", "pd")
#     # Add model name and re-order columns
#     #df$Model <- mnames[n]
#     #df <- df[, c(9, 1:8)]
#     
#     # Get current model stats for lmer model
#   } else {
#     df <- data.frame(summary(model$coefficients))
#     df$Parameter <- rownames(df)
#     df <- df[, c(7, 6, 1:5)]
#   }
#   df <- df |> 
#     mutate(across(where(is.numeric), \(x) round(x, 2))) |>
#     # mutate(Rhat = round(Rhat, 3)) |>
#     tibble::remove_rownames() |> 
#     # Replace "bandInt" with "Band" wherever it occurs in the Term column
#     mutate(Term = stringr::str_replace_all(Term, "bandInt", "Band")) |>
#     # Remove "b_" from the start of the Term column if it's present
#     mutate(Term = stringr::str_replace_all(Term, "^b_", ""),
#            Term = stringr::str_replace_all(Term,"conditVaried:Band","condit*Band")) 
#   return(df)
# }


#GetModelStats(e1_vxBMM) |> kable(booktabs = TRUE)



s1_agg |> group_by(refClass) |>
  summarise(n=n(),sum=sum(close_match), pct=sum/n)

s1_agg |> group_by(id,refClass) |>
  summarise(n=n(),sum=sum(close_match), pct=sum/n) |> 
  group_by(refClass) |> summarise(mean_pct=median(pct),median_pct=median(pct),sd_pct=sd(pct),nonzero=sum(pct>0),n=n())

s1_agg |> group_by(id,refClass) |>
      summarise(n=n(),sum=sum(close_match), pct=sum/n) |> arrange(-pct)



m4_close <- brm(
  close_match ~ refClass + (1|id),
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg_close.rds')),
  family = bernoulli()
) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.29      0.52    -0.69     1.35 1.00      859     1749
# refClassPercentage    -1.55      0.78    -3.14    -0.10 1.00      643     1395
# refClassUSD           -3.68      0.80    -5.35    -2.16 1.00      948     1551

m4_close |> emmeans(~refClass, type="response")
#  refClass   response lower.HPD upper.HPD
#  kWh            0.57      0.34      0.79
#  Percentage     0.22      0.06      0.41
#  USD            0.03      0.01      0.08

m4_close_c <- brm(
  close_match ~ refClass + calc + (1|id),
  data = s1_agg,
  file=paste0(here::here("data/model_cache",'s1_mg_close_c.rds')),
  family = bernoulli()
) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -1.79      0.79    -3.41    -0.27 1.00     1292     1933
# refClassPercentage    -1.70      0.78    -3.30    -0.23 1.00     1143     1879
# refClassUSD           -3.62      0.80    -5.29    -2.16 1.00     1262     1873
# calcUsedCalculator     2.69      0.76     1.27     4.22 1.00     1265     2022

m4_close_c |> emmeans(~refClass, type="response")
#  refClass   response lower.HPD upper.HPD
#  kWh            0.39     0.168      0.65
#  Percentage     0.11     0.023      0.24
#  USD            0.02     0.002      0.04


s1_bb1_close <- brm(
  successes | trials(4) ~ refClass,
  data = s1_agg |> 
    group_by(id,refClass) |> 
    summarise(successes = sum(close_match), trials = n()),
  family = beta_binomial(),
  file=paste0(here::here("data/model_cache",'s1_bb1_close.rds'))
)
summary(s1_bb1_close)
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.10      0.01     0.07     0.12 1.00     3184     3134
# refClassPercentage    -0.55      0.02    -0.58    -0.52 1.00     3211     2901
# refClassUSD           -1.32      0.02    -1.36    -1.29 1.00     3431     2999

s1_bb1_close |> emmeans(~refClass, type="response")



s1_zib_close <- brm(
  successes | trials(trials) ~ refClass + (1|id),
  data = s1_agg |> 
    group_by(id,refClass) |> 
    summarise(successes = sum(close_match), trials = n()),
  family = zero_inflated_binomial(),
  file=paste0(here::here("data/model_cache",'s1_zib_close.rds'))
)
summary(s1_zib_close)
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              1.42      0.77    -0.07     2.90 1.01      313     1034
# refClassPercentage    -1.90      0.78    -3.44    -0.40 1.00     1057     1972
# refClassUSD           -3.91      0.79    -5.56    -2.43 1.00     1619     2194

# Further Distributional Parameters:
#    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# zi     0.17      0.08     0.02     0.32 1.02      229      451

s1_zib_close |> emmeans(~refClass, type="response")
#  refClass   prob lower.HPD upper.HPD
#  kWh        0.81      0.53      0.97
#  Percentage 0.39      0.14      0.65
#  USD        0.08      0.01      0.20



s1_zib_close_c <- brm(
  successes | trials(trials) ~ refClass + calc + (1|id),
  data = s1_agg |> 
    group_by(id,refClass,calc) |> 
    summarise(successes = sum(close_match), trials = n()),
  family = zero_inflated_binomial(),
   control = list(adapt_delta = .94, max_treedepth = 13),
  file=paste0(here::here("data/model_cache",'s1_zib_close_c.rds'))
)
summary(s1_zib_close_c)
Regression Coefficients:
                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
Intercept             -0.88      0.93    -2.74     0.95 1.01      439     1421
refClassPercentage    -1.90      0.78    -3.49    -0.43 1.01      783     1322
refClassUSD           -3.78      0.80    -5.47    -2.30 1.00      908     1529
calcUsedCalculator     2.59      0.76     1.16     4.10 1.01     1070     1957

Further Distributional Parameters:
   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
zi     0.13      0.07     0.01     0.27 1.02      216      774

s1_zib_close_c |> emmeans(~refClass, type="response")
#  refClass   prob lower.HPD upper.HPD
#  kWh        0.60     0.276      0.88
#  Percentage 0.19     0.040      0.40
#  USD        0.03     0.003      0.10


# Computed from 4000 by 252 log-likelihood matrix.

#           Estimate   SE
# elpd_waic   -261.1  9.7
# p_waic        99.1  2.6
# waic         522.2 19.5

# 134 (53.2%) p_waic estimates greater than 0.4. We recommend trying loo instead.



# S2


s2_agg |> group_by(refClass) |>
  summarise(n=n(),sum=sum(close_match), pct=sum/n)

s2_agg |> group_by(id,refClass) |>
  summarise(n=n(),sum=sum(close_match), pct=sum/n) |> 
  group_by(refClass) |> summarise(mean_pct=median(pct),median_pct=median(pct),sd_pct=sd(pct),nonzero=sum(pct>0),n=n())

s2_agg |> group_by(id,refClass,calc) |>
  summarise(n=n(),sum=sum(close_match), pct=sum/n) |> 
  group_by(calc,refClass) |> summarise(mean_pct=median(pct),median_pct=median(pct),sd_pct=sd(pct),nonzero=sum(pct>0),n=n())

s2_agg |> group_by(id,refClass,calc) |>
  summarise(n=n(),sum=sum(close_match2), pct=sum/n) |> 
  group_by(calc,refClass) |> summarise(mean_pct=median(pct),median_pct=median(pct),sd_pct=sd(pct),nonzero=sum(pct>0),n=n())



s2_close_c <- brm(
  close_match ~ refClass + calc + (1|id),
  data = s2_agg,
  file=paste0(here::here("data/model_cache",'s2_mg_close_c.rds')),
  family = bernoulli()
) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              1.01      0.75    -0.42     2.53 1.00     1403     2239
# refClassPercentage    -1.45      0.98    -3.46     0.43 1.00     1350     2066
# refClassUSD           -3.15      1.10    -5.38    -1.06 1.00     1418     2258
# calcNoCalculator      -5.39      1.32    -8.16    -3.02 1.00     1683     2107


s2_close_c |> emmeans(~refClass, type="response")
#  refClass   response lower.HPD upper.HPD
#  kWh           0.162    0.0176      0.42
#  Percentage    0.043    0.0022      0.14
#  USD           0.008    0.0003      0.04


s2_close_c_full <- brm(
  close_match ~ refClass + calc + rounded + pct_goal + (1|id),
  data = s2_agg,
  file=paste0(here::here("data/model_cache",'s2_mg_close_c_full.rds')),
  family = bernoulli()
) 
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.31      0.82    -1.25     1.95 1.00     1265     1996
# refClassPercentage    -1.53      1.05    -3.64     0.50 1.00     1211     1822
# refClassUSD           -3.36      1.20    -5.85    -1.12 1.00     1183     1771
# calcNoCalculator      -5.78      1.44    -8.81    -3.23 1.00     1538     2177
# roundedRounded         0.82      0.28     0.28     1.39 1.00     8974     3198
# pct_goal15%            0.73      0.29     0.17     1.31 1.00     8505     3172



s2_close_cint <- brm(
  close_match ~ refClass*calc + (1|id),
  data = s2_agg,
  file=paste0(here::here("data/model_cache",'s2_mg_close_cint.rds')),
  family = bernoulli()
) 

s2_close_cint |> emmeans(~refClass*calc, type="response")
#  refClass   calc          response lower.HPD upper.HPD
#  kWh        Calculator        0.78      0.50      0.98
#  Percentage Calculator        0.35      0.08      0.65
#  USD        Calculator        0.11      0.01      0.36
#  kWh        No Calculator     0.00      0.00      0.06
#  Percentage No Calculator     0.01      0.00      0.21
#  USD        No Calculator     0.00      0.00      0.01



s2_abs_error <- brm(
  formula = abs_error ~ refClass * calc + (1 | id),
  data = s2_agg,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "b"),       # Prior for fixed effects
    prior(normal(0, 5), class = "Intercept") # Prior for intercept
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 123
)
s2_abs_error |> emmeans(~refClass*calc, type="response")



s2_abs_error_log <- brm(
  formula = log_abs_error ~ refClass * calc + (1 | id),
  data = s2_agg |> mutate(log_abs_error=log(.01+abs_error*100)),
  family = gaussian(),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 123
)
# Regression Coefficients:
#                                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                              -1.19      0.37    -1.94    -0.47 1.00     1131     2647
# refClassPercentage                      1.19      0.53     0.17     2.23 1.01     1076     2309
# refClassUSD                             1.86      0.56     0.77     2.95 1.00     1242     2667
# calcNoCalculator                        3.38      0.85     1.72     5.01 1.00     1665     3399
# refClassPercentage:calcNoCalculator    -1.69      1.24    -4.11     0.77 1.00     1495     3137
# refClassUSD:calcNoCalculator           -1.08      1.23    -3.49     1.38 1.00     1575     2923

# Further Distributional Parameters:
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     1.59      0.05     1.50     1.68 1.00    10916     8877


# s2_agg |> mutate(log_abs_error=log(.01+abs_error*100)) |> mutate(undo = (exp(log_abs_error)/100)-.01) |> select(-edu,-block,-total_kWh)

s2_abs_error_log |> emmeans(~refClass*calc, type="response")
#  refClass   calc          emmean lower.HPD upper.HPD
#  kWh        Calculator     -1.18     -1.94      -0.5
#  Percentage Calculator      0.00     -0.75       0.7
#  USD        Calculator      0.68     -0.15       1.5
#  kWh        No Calculator   2.20      0.69       3.6
#  Percentage No Calculator   1.69      0.10       3.4
#  USD        No Calculator   2.97      1.46       4.6


patchwork::wrap_plots(plot(conditional_effects(s2_abs_error_log),points=FALSE,plot=FALSE))

# plot with data reverse transformed from the  mutate(log_abs_error=log(.01+abs_error*100)) applied to the data, so that the scale is interpretable
s2_abs_error |>  emmeans( ~refClass, type="response") |>
  gather_emmeans_draws() |> 
  mutate(orig = (orig_scale=exp(.value)/100)-.01) |>
  ggplot(aes(x = refClass, y = orig)) +
  geom_point(aes(color = refClass), position = position_jitter(width = 0.1)) +
  stat_halfeye(alpha = 0.2) 



