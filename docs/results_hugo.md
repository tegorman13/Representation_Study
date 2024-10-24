---
title: Results
author:
  - name: Thomas E. Gorman
    url: https://tegorman13.github.io/
    affiliations:
      - name: Communication and Cognition Lab, Purdue University, USA
        affiliation-url: https://web.ics.purdue.edu/~treimer/
lightbox: true
toc: false
code-fold: true
code-tools: true
execute:
  warning: false
format:
  html:
    grid:
      sidebar-width: 220px
      body-width: 1200px
      margin-width: 170px
      gutter-width: 1.0rem
  hugo-md:
    include: true
    html-math-method: mathjax
    output-file: results_hugo.md
  gfm:
    echo: true
    output-file: results_gfm.md
---


<details class="code-fold">
<summary>Code</summary>

``` r
library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here,tibble,brms,rstan,bayestestR,emmeans,tidybayes,modelsummary,
               ggplot2,gt,knitr,kableExtra,ggh4x,lme4,flextable)

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
```

</details>

## Experiment 1

<details class="code-fold">
<summary>Code</summary>

``` r
s1_bb1 <- brm(
  mg | trials(4) ~ refClass,
  family = beta_binomial(),
  cores=4,
  iter=5000,
  data = s1_agg4,
  file=paste0(here::here("data/model_cache",'s1_bb1.rds'))
)

mted1 <- as.data.frame(describe_posterior(s1_bb1, centrality = "Mean"))[, c(1,2,4,5,6)]
colnames(mted1) <- c("Term", "Estimate","95%\\nCrI Lower", "95%\\nCrI Upper", "pd")

mted1 |> mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  tibble::remove_rownames() |>
  mutate(Term = stringr::str_remove(Term, "b_")) |> kable(booktabs = TRUE)
```

</details>

| Term               | Estimate | 95%Lower | 95%Upper |  pd |
|:-------------------|---------:|---------:|---------:|----:|
| Intercept          |    -0.58 |    -0.61 |    -0.56 |   1 |
| refClassPercentage |    -0.70 |    -0.74 |    -0.66 |   1 |
| refClassUSD        |    -1.74 |    -1.78 |    -1.69 |   1 |
