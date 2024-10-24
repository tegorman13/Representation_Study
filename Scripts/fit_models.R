
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
            pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), state_dif=mean(state_dif)) |> 
  mutate(matched_goal = (pct_change == pct_goal))

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
         matched_goal2 = abs(pct_change-pct)<.03)

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