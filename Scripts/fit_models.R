
library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here,tibble,brms,bayestestR, ggplot2,gt,ggh4x,lme4,flextable)

options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)
options(brms.backend="cmdstanr",mc.cores=4)





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