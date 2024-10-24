---
title: Study 2 Analysis
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
cache: true
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
    output-file: s2_hugo.md
  gfm:
    echo: false
    output-file: s2_gfm.md
---


<link href="site_libs/htmltools-fill-0.5.8.1/fill.css" rel="stylesheet" />
<script src="site_libs/htmlwidgets-1.6.4/htmlwidgets.js"></script>
<link href="site_libs/datatables-css-0.0.0/datatables-crosstalk.css" rel="stylesheet" />
<script src="site_libs/datatables-binding-0.33/datatables.js"></script>
<script src="site_libs/jquery-3.6.0/jquery-3.6.0.min.js"></script>
<link href="site_libs/dt-core-1.13.6/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="site_libs/dt-core-1.13.6/css/jquery.dataTables.extra.css" rel="stylesheet" />
<script src="site_libs/dt-core-1.13.6/js/jquery.dataTables.min.js"></script>
<script src="site_libs/jszip-1.13.6/jszip.min.js"></script>
<script src="site_libs/pdfmake-1.13.6/pdfmake.js"></script>
<script src="site_libs/pdfmake-1.13.6/vfs_fonts.js"></script>
<link href="site_libs/dt-ext-buttons-1.13.6/css/buttons.dataTables.min.css" rel="stylesheet" />
<script src="site_libs/dt-ext-buttons-1.13.6/js/dataTables.buttons.min.js"></script>
<script src="site_libs/dt-ext-buttons-1.13.6/js/buttons.html5.min.js"></script>
<script src="site_libs/dt-ext-buttons-1.13.6/js/buttons.colVis.min.js"></script>
<script src="site_libs/dt-ext-buttons-1.13.6/js/buttons.print.min.js"></script>
<link href="site_libs/nouislider-7.0.10/jquery.nouislider.min.css" rel="stylesheet" />
<script src="site_libs/nouislider-7.0.10/jquery.nouislider.min.js"></script>
<link href="site_libs/selectize-0.12.0/selectize.bootstrap3.css" rel="stylesheet" />
<script src="site_libs/selectize-0.12.0/selectize.min.js"></script>
<link href="site_libs/crosstalk-1.2.1/css/crosstalk.min.css" rel="stylesheet" />
<script src="site_libs/crosstalk-1.2.1/js/crosstalk.min.js"></script>


<details class="code-fold">
<summary>Code</summary>

``` r
library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here, haven,tibble,ggplot2,gt,ggh4x,lme4,flextable)

options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)
walk(c("fun_plot"), ~ source(here::here(paste0("Scripts/", .x, ".R"))))
theme_set(theme_nice())



s2 <- haven::read_sav("data/Frequency & Probability Study 2 - 3-24-19.sav")
s2 <- s2 |> mutate(id = as.numeric(format(EndDate, "%H%M%S"))) |> relocate(id) 
s2 <- s2 |> filter(!(id==203309)) # NA for some responses
s2 <- s2 |> 
  mutate(
    state1 = case_when(
      COLORADO == 1 ~ "Colorado",
      MASSACHUSETTS == 1 ~ "Massachusetts",
      CALIFORNIA == 1 ~ "California",
      TEXAS == 1 ~ "Texas",
      TRUE ~ NA_character_
    ),
    state2 = case_when(
      COLORADO == 2 ~ "Colorado",
      MASSACHUSETTS == 2 ~ "Massachusetts",
      CALIFORNIA == 2 ~ "California",
      TEXAS == 2 ~ "Texas",
      TRUE ~ NA_character_
    )
  ) |> 
  mutate(refClass = case_when(
  REFERENCECLASS == 1 ~ "USD",
  REFERENCECLASS == 2 ~ "kWh",
  REFERENCECLASS == 3 ~ "Percentage"
)) |> 
  rename(calc=MATH01) |>  # replace NA's in calc with 0's
  mutate(calc=factor(ifelse(is.na(calc),0,calc))) |> 
  relocate(refClass,state1,state2,calc, .after = id) 

s2 <- s2 |> rename(edu=DEM04) |>
  mutate(edu = factor(case_when(
    edu == '1' ~ "Some schooling, but no diploma or degree",
    edu == 2 ~ "Highschool diploma or GED",
    edu == 3 ~ "Some college",
    edu == 4 ~ "College degree",
    edu == 5 ~ "Some graduate school",
    edu == 6 ~ "Graduate degree",
    edu == 7 ~ "Choose not to answer",
    TRUE ~ NA_character_
  ))) |> relocate(edu, .after = id) 
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
s2_b1 <- s2 |> select(id,state1,refClass,TENPERCENT,ROUNDED, calc, edu,ACFAMILY1:TOTALAPTWO1) |> 
  mutate( 
    block = 1,
    state=state1,
  pct_goal=case_when (TENPERCENT == 1 ~ "10%", TENPERCENT == 2 ~ "15%"), 
  pct=case_when (TENPERCENT == 1 ~ .10, TENPERCENT == 2 ~ .15),
  rounded = case_when(ROUNDED == 1 ~ "Rounded", ROUNDED == 2 ~ "Not Rounded")
  ) |> relocate(id,block,refClass,pct_goal,rounded,state)  |> 
  select(-TENPERCENT,-ROUNDED, -state1)

s2_b2 <- s2 |> select(id,state2,refClass,TENPERCENT,ROUNDED, calc,edu,ACFAMILY2:TOTALAPTWO2) |> 
  mutate( 
    block = 2,
    state=state2,
  pct_goal=case_when (TENPERCENT == 1 ~ "15%", TENPERCENT == 2 ~ "10%"), 
  pct=case_when (TENPERCENT == 1 ~ .15, TENPERCENT == 2 ~ .10),
  rounded = case_when(ROUNDED == 1 ~ "Not Rounded", ROUNDED == 2 ~ "Rounded")
  ) |> relocate(id,block,refClass,pct_goal,rounded,state) |> 
  select(-TENPERCENT,-ROUNDED, -state2)

s2_b1_long <- s2_b1 |>
  pivot_longer(
    cols = c(ACFAMILY1:TOTALAPTWO1),
    names_to = c("appliance", "measure"),
    names_pattern = "(AC|HEAT|WATER|REFRIG|OTHER|TOTAL)(FAMILY1|STATE1|APONE1|APTWO1)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |>
  rename(
    family = FAMILY1,
    state_avg = STATE1,
    plan1 = APONE1,
    plan2 = APTWO1
  ) |>
  relocate(appliance, .after = refClass)

s2_b2_long <- s2_b2 |>
  pivot_longer(
    cols = c(ACFAMILY2:TOTALAPTWO2),
    names_to = c("appliance", "measure"),
    names_pattern = "(AC|HEAT|WATER|REFRIG|OTHER|TOTAL)(FAMILY2|STATE2|APONE2|APTWO2)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |>
  rename(
    family = FAMILY2,
    state_avg = STATE2,
    plan1 = APONE2,
    plan2 = APTWO2
  ) |>
  relocate(appliance, .after = refClass)


s2_long <- bind_rows(s2_b1_long, s2_b2_long) |> 
  arrange(id, block) 

# pivot so that plan1 and plan2 are on their own rows
s2_long <- s2_long |>
  group_by(id,block) |> 
  pivot_longer(
    cols = c(plan1, plan2),
    names_to = "plan",
    values_to = "value"
  ) |> 
  mutate(change = value-family, state_dif = value-state_avg,plan=as_factor(plan)) 


#write.csv(s2_long,"data/study_2_tg.csv")
saveRDS(s2_long, here::here("data/s2_processed.rds"))
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
s2_long <- readRDS(here::here("data/s2_processed.rds"))

# compute total kWh reduction, and total % reduction 
s2_agg <- s2_long |> 
  filter(appliance !="TOTAL") |> 
  group_by(id,refClass,state,pct_goal,pct,rounded,block,plan,calc,edu) |> 
  summarise(total_kWh = sum(value),orig_kWh=sum(family), pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), state_dif=mean(state_dif))


# Step 1: Calculate whether participants matched the target pct_goal
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
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> group_by(id,refClass) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
  group_by(refClass) |>
  summarise(mean_pct=mean(pct), sd_pct=sd(pct), n=n()) 
```

</details>

    # A tibble: 3 × 4
      refClass   mean_pct sd_pct     n
      <chr>         <dbl>  <dbl> <int>
    1 Percentage    0.26   0.345    75
    2 USD           0.179  0.371    60
    3 kWh           0.423  0.446    71

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> group_by(id,refClass) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
  group_by(refClass,mg) |>
  summarise(n=n()) 
```

</details>

    # A tibble: 15 × 3
    # Groups:   refClass [3]
       refClass      mg     n
       <chr>      <int> <int>
     1 Percentage     0    43
     2 Percentage     1     4
     3 Percentage     2    18
     4 Percentage     3     2
     5 Percentage     4     7
     6 Percentage     8     1
     7 USD            0    48
     8 USD            2     2
     9 USD            3     1
    10 USD            4     9
    11 kWh            0    34
    12 kWh            1     3
    13 kWh            2     6
    14 kWh            3     7
    15 kWh            4    21

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> group_by(id,refClass) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
  ggplot(aes(x=mg)) +
  geom_histogram(aes(fill=refClass), alpha=0.5, bins=20) +
  ggdist::stat_halfeye() +
  geom_rug(alpha=0.1) +
  stat_boxplot(width=0.5, alpha=0.6) +
  facet_wrap(~refClass) +
  labs(title="Distribution of Percent Change by State and Reference Class - Before outlier exclusion",
       x="Percent Change",
       y="Density") +
  scale_fill_brewer(palette="Set2") + 
  scale_y_continuous(breaks=seq(0,80,5))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> group_by(id,refClass,pct_goal) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
  ggplot(aes(x=mg)) +
  geom_histogram(aes(fill=refClass), alpha=0.5, bins=20) +
  ggdist::stat_halfeye() +
  geom_rug(alpha=0.1) +
  stat_boxplot(width=0.5, alpha=0.6) +
  facet_wrap(pct_goal~refClass) +
  labs(title="Distribution of Percent Change by State and Reference Class - Before outlier exclusion",
       x="Percent Change",
       y="Density") +
  scale_fill_brewer(palette="Set2") + 
  scale_y_continuous(breaks=seq(0,80,5))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-4-2.png" width="768" />

## Inspect response distribution for outliers

<details class="code-fold">
<summary>Code</summary>

``` r
outliers <- s2_agg |>  ungroup() |> group_by(state) |> mutate(change_mean=mean(pct_change,na.rm=TRUE),change_sd=sd(pct_change,na.rm=TRUE),
                                     z_score=(pct_change-change_mean)/change_sd,
                                     is_outlier=abs(z_score)>2.75)

outlier_id <- outliers |> filter(is_outlier) |> pull(id) |> unique()

outlier_summary <- outliers %>%
  group_by(pct_goal, rounded, refClass,state) %>%
  summarise(
    num_outliers = sum(is_outlier, na.rm = TRUE),
    total = n(),
    outlier_percentage = (num_outliers / total) * 100
  )

outlier_plot <- ggplot(outliers, aes(x = state, y = z_score, color = is_outlier)) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~ pct_goal) +
  labs(
    title = "Z-Scores of Change in Energy Usage by State - outliers colored",
    x = "State",
    y = "Z-Score",
    color = "Outlier"
  ) +
  theme_minimal()

# Assess skewness and normality of the data
library(e1071)
# Calculate skewness and kurtosis for the `pct_change` variable
skewness_value <- skewness(s2_agg$pct_change, na.rm = TRUE)
kurtosis_value <- kurtosis(s2_agg$pct_change, na.rm = TRUE)

cat("Skewness:", skewness_value, "\n")
```

</details>

    Skewness: 1.2 

<details class="code-fold">
<summary>Code</summary>

``` r
cat("Kurtosis:", kurtosis_value, "\n")
```

</details>

    Kurtosis: 3 

<details class="code-fold">
<summary>Code</summary>

``` r
# Shapiro-Wilk test for normality (sample size limitation)
shapiro_test <- shapiro.test(s2_agg$pct_change[1:5000])  # Using first 5000 observations if large dataset
print(shapiro_test)
```

</details>


        Shapiro-Wilk normality test

    data:  s2_agg$pct_change[1:5000]
    W = 0.8, p-value <0.0000000000000002

## Before and after excluding outliers

<details class="code-fold">
<summary>Code</summary>

``` r
#outlier_plot

s2_agg |> ggplot(aes(x=pct_change)) +
  geom_density(aes(fill=state), alpha=0.5) +
  ggdist::stat_halfeye() +
  geom_rug(alpha=0.1) +
  stat_boxplot(width=0.5, alpha=0.6) +
  facet_wrap(~refClass) +
  labs(title="Distribution of Percent Change by State and Reference Class - Before outlier exclusion",
       x="Percent Change",
       y="Density") +
  scale_fill_brewer(palette="Set2")
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="864" />

<details class="code-fold">
<summary>Code</summary>

``` r
# plot densities in a better way to account for the positive skew

s2_agg <- s2_agg |> 
  filter(!id %in% outlier_id)

s2_agg |> 
  filter(!id %in% outlier_id) |>
  ggplot(aes(x=pct_change)) +
  geom_density(aes(fill=state), alpha=0.5) +
  ggdist::stat_halfeye() +
  geom_rug(alpha=0.1) +
  stat_boxplot(width=0.5, alpha=0.6) +
  facet_wrap(~refClass) +
  labs(title="After outlier exclusion",
       x="Percent Change",
       y="Density") +
  scale_fill_brewer(palette="Set2")
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-2.png" width="864" />

## Individual Participant responses

-   Facets indicate the Reference class (%, kWh, USD) and the goal % reduction (10%, 15%)
-   dashed lines also indicate goal percentage - dots on the line indicate participants who met the goal

<details class="code-fold">
<summary>Code</summary>

``` r
  # add geom_vline with x intercept based on pct_goal of the data (which is currently a string of form 10%)
s2_agg |> 
  filter(block==1) |>
  ungroup() |> 
  mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100) |>
  mutate(id=reorder(id,pct_change)) |> 
  ggplot(aes(y=id,x=pct_change)) + 
  geom_point(size=1,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
  geom_vline(aes(xintercept=goal_pct),linetype="dashed",alpha=.5) +
  ggh4x::facet_nested_wrap(refClass~pct_goal,axes="all",scales="free",ncol=2)  + 
  labs(y="Participant Id", x="Percent Change", title="Individual Performance") +
  theme(axis.text.y = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-1.png" width="960" />

## Interactive data table

<details class="code-fold">
<summary>Code</summary>

``` r
# plot distribution of reductions by applicance
# s2_long |> ggplot(aes(x = change)) + geom_histogram(bins = 30) + facet_wrap(~appliance, scales = "free")
s2_agg |> mutate(refClass=factor(refClass),pct_goal=factor(pct_goal),rounded=factor(rounded),state=factor(state)) |>
DT::datatable(extensions = 'Buttons',
  options = list(
    dom = 'Blfrtip',
    buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
    pageLength = 7,
    autoWidth = TRUE
  ),
  filter = 'top') # Add filtering at the top of each column)
```

</details>
<div class="datatables html-widget html-fill-item" id="htmlwidget-eeb183c20bd7f42cbf67" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-eeb183c20bd7f42cbf67">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"5151\" data-max=\"223715\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;kWh&quot;,&quot;Percentage&quot;,&quot;USD&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;California&quot;,&quot;Colorado&quot;,&quot;Massachusetts&quot;,&quot;Texas&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;10%&quot;,&quot;15%&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.1\" data-max=\"0.15\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Not Rounded&quot;,&quot;Rounded&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;plan1&quot;,&quot;plan2&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;0&quot;,&quot;1&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;College degree&quot;,&quot;Graduate degree&quot;,&quot;Highschool diploma or GED&quot;,&quot;Some college&quot;,&quot;Some graduate school&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"3215\" data-max=\"51000\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"19516\" data-max=\"46293\" data-scale=\"11\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.354\" data-max=\"0.877\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-5305.4\" data-max=\"4093\" data-scale=\"14\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"logical\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;true&quot;,&quot;false&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"logical\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;true&quot;,&quot;false&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480","481","482","483","484","485","486","487","488","489","490","491","492","493","494","495","496","497","498","499","500","501","502","503","504","505","506","507","508","509","510","511","512","513","514","515","516","517","518","519","520","521","522","523","524","525","526","527","528","529","530","531","532","533","534","535","536","537","538","539","540","541","542","543","544","545","546","547","548","549","550","551","552","553","554","555","556","557","558","559","560","561","562","563","564","565","566","567","568","569","570","571","572","573","574","575","576","577","578","579","580","581","582","583","584","585","586","587","588","589","590","591","592","593","594","595","596","597","598","599","600","601","602","603","604","605","606","607","608","609","610","611","612","613","614","615","616","617","618","619","620","621","622","623","624","625","626","627","628","629","630","631","632","633","634","635","636","637","638","639","640","641","642","643","644","645","646","647","648","649","650","651","652","653","654","655","656","657","658","659","660","661","662","663","664","665","666","667","668","669","670","671","672","673","674","675","676","677","678","679","680","681","682","683","684","685","686","687","688","689","690","691","692","693","694","695","696","697","698","699","700","701","702","703","704","705","706","707","708","709","710","711","712","713","714","715","716","717","718","719","720","721","722","723","724","725","726","727","728","729","730","731","732","733","734","735","736","737","738","739","740","741","742","743","744","745","746","747","748","749","750","751","752","753","754","755","756","757","758","759","760","761","762","763","764","765","766","767","768","769","770","771","772","773","774","775","776","777","778","779","780"],[5151,5151,5151,5151,74040,74040,74040,74040,74312,74312,74312,74312,74440,74440,74440,74440,74709,74709,74709,74709,74729,74729,74729,74729,74827,74827,74827,74827,74940,74940,74940,74940,75022,75022,75022,75022,75029,75029,75029,75029,75037,75037,75037,75037,75038,75038,75038,75038,75234,75234,75234,75234,75305,75305,75305,75305,75549,75549,75549,75549,75616,75616,75616,75616,75632,75632,75632,75632,75633,75633,75633,75633,75646,75646,75646,75646,75819,75819,75819,75819,75929,75929,75929,75929,80151,80151,80151,80151,80208,80208,80208,80208,80210,80210,80210,80210,80424,80424,80424,80424,80447,80447,80447,80447,80532,80532,80532,80532,80536,80536,80536,80536,80635,80635,80635,80635,80701,80701,80701,80701,80909,80909,80909,80909,81659,81659,81659,81659,81915,81915,81915,81915,82044,82044,82044,82044,82424,82424,82424,82424,82732,82732,82732,82732,83053,83053,83053,83053,83110,83110,83110,83110,83747,83747,83747,83747,83926,83926,83926,83926,84104,84104,84104,84104,84113,84113,84113,84113,84400,84400,84400,84400,84629,84629,84629,84629,84914,84914,84914,84914,85429,85429,85429,85429,90109,90109,90109,90109,91308,91308,91308,91308,91424,91424,91424,91424,91443,91443,91443,91443,91449,91449,91449,91449,91510,91510,91510,91510,91821,91821,91821,91821,92032,92032,92032,92032,92712,92712,92712,92712,93009,93009,93009,93009,93439,93439,93439,93439,93840,93840,93840,93840,93951,93951,93951,93951,94026,94026,94026,94026,94642,94642,94642,94642,101457,101457,101457,101457,102246,102246,102246,102246,102356,102356,102356,102356,102402,102402,102402,102402,102644,102644,102644,102644,102746,102746,102746,102746,102846,102846,102846,102846,102940,102940,102940,102940,103110,103110,103110,103110,103120,103120,103120,103120,103132,103132,103132,103132,103443,103443,103443,103443,103616,103616,103616,103616,103712,103712,103712,103712,103955,103955,103955,103955,104405,104405,104405,104405,104415,104415,104415,104415,104614,104614,104614,104614,104630,104630,104630,104630,104733,104733,104733,104733,105331,105331,105331,105331,105808,105808,105808,105808,110923,110923,110923,110923,110943,110943,110943,110943,111702,111702,111702,111702,112242,112242,112242,112242,112917,112917,112917,112917,113011,113011,113011,113011,113020,113020,113020,113020,113029,113029,113029,113029,113521,113521,113521,113521,114222,114222,114222,114222,114435,114435,114435,114435,114649,114649,114649,114649,115300,115300,115300,115300,115535,115535,115535,115535,115928,115928,115928,115928,120258,120258,120258,120258,120707,120707,120707,120707,120933,120933,120933,120933,121726,121726,121726,121726,122858,122858,122858,122858,123220,123220,123220,123220,123825,123825,123825,123825,124450,124450,124450,124450,125803,125803,125803,125803,130522,130522,130522,130522,130601,130601,130601,130601,163448,163448,163448,163448,163838,163838,163838,163838,164000,164000,164000,164000,164003,164003,164003,164003,164237,164237,164237,164237,164644,164644,164644,164644,165531,165531,165531,165531,165833,165833,165833,165833,171812,171812,171812,171812,171846,171846,171846,171846,171856,171856,171856,171856,172037,172037,172037,172037,172549,172549,172549,172549,173505,173505,173505,173505,174947,174947,174947,174947,181349,181349,181349,181349,184138,184138,184138,184138,191615,191615,191615,191615,195603,195603,195603,195603,195846,195846,195846,195846,200049,200049,200049,200049,200150,200150,200150,200150,200202,200202,200202,200202,200235,200235,200235,200235,200456,200456,200456,200456,200515,200515,200515,200515,200602,200602,200602,200602,200629,200629,200629,200629,200729,200729,200729,200729,200750,200750,200750,200750,200815,200815,200815,200815,200833,200833,200833,200833,200843,200843,200843,200843,200856,200856,200856,200856,200911,200911,200911,200911,200913,200913,200913,200913,200921,200921,200921,200921,201014,201014,201014,201014,201028,201028,201028,201028,201113,201113,201113,201113,201300,201300,201300,201300,201528,201528,201528,201528,201546,201546,201546,201546,201551,201551,201551,201551,201623,201623,201623,201623,201630,201630,201630,201630,201647,201647,201647,201647,201655,201655,201655,201655,201707,201707,201707,201707,202009,202009,202009,202009,202009,202009,202009,202009,202119,202119,202119,202119,202218,202218,202218,202218,202228,202228,202228,202228,202504,202504,202504,202504,202520,202520,202520,202520,202552,202552,202552,202552,202653,202653,202653,202653,202737,202737,202737,202737,202758,202758,202758,202758,202916,202916,202916,202916,203100,203100,203100,203100,203104,203104,203104,203104,203153,203153,203153,203153,203234,203234,203234,203234,203542,203542,203542,203542,203635,203635,203635,203635,203810,203810,203810,203810,203822,203822,203822,203822,203829,203829,203829,203829,203910,203910,203910,203910,204012,204012,204012,204012,204056,204056,204056,204056,204201,204201,204201,204201,204745,204745,204745,204745,204751,204751,204751,204751,205019,205019,205019,205019,205352,205352,205352,205352,205438,205438,205438,205438,205813,205813,205813,205813,210131,210131,210131,210131,211120,211120,211120,211120,212036,212036,212036,212036,213526,213526,213526,213526,214040,214040,214040,214040,223715,223715,223715,223715],["USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","kWh","kWh","kWh","kWh","USD","USD","USD","USD","USD","USD","USD","USD","kWh","kWh","kWh","kWh","Percentage","Percentage","Percentage","Percentage","USD","USD","USD","USD"],["California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Colorado","Colorado","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Massachusetts","Massachusetts","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Colorado","Colorado","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Massachusetts","Massachusetts","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas","California","California","Massachusetts","Massachusetts","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","California","California","Colorado","Colorado","Massachusetts","Massachusetts","Texas","Texas","Colorado","Colorado","Texas","Texas"],["15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","15%","15%","10%","10%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%","10%","10%","15%","15%","10%","10%","15%","15%","15%","15%","10%","10%","10%","10%","15%","15%"],[0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15],["Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded","Not Rounded","Not Rounded","Rounded","Rounded","Rounded","Rounded","Not Rounded","Not Rounded"],[2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2],["plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2","plan1","plan2"],["1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","0","0","0","0","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1"],["College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","Highschool diploma or GED","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","College degree","Graduate degree","Graduate degree","Graduate degree","Graduate degree","College degree","College degree","College degree","College degree","Some college","Some college","Some college","Some college","Graduate degree","Graduate degree","Graduate degree","Graduate degree","Some graduate school","Some graduate school","Some graduate school","Some graduate school"],[3215,3215,13793,13793,46001,50001,27000,27000,33803,33803,26100,26100,17000,17000,28300,28300,19550,19550,35791,35791,20700,20700,33803,33803,34700,40000,24650,25650,18200,20000,39000,36000,19200,19200,30000,30000,26300,26000,19500,19300,41400,41400,25689,24113,33500,30100,18000,18000,41200,41200,24692,24692,18000,13100,41300,21750,19000,23400,18000,25000,40000,40000,24689,24689,3542,5000,20035,30000,46002,32135,29000,22950,21803,24098,15635,17362,19200,19200,34000,34000,35200,36200,24500,23500,31400,27400,20200,16500,39349,39349,26100,26100,30450,28750,28500,26900,34000,34000,25827,27188,20100,20300,40950,41100,31001,29301,22000,21000,44530,37340,29000,22330,37300,38650,25500,24650,20325,19250,41400,41400,18543,17626,43347,29506,21850,16700,30000,28300,27700,26700,17700,17700,20700,20700,39349,39349,39200,39200,25700,26100,15050,7025,10950,5025,19300,19500,41000,35950,20000,19170,32868,28843,14050.5,19545,41666,18565.124,20610,9955,12801,6220,41400,41400,24689,24689,13200,9700,9500,5680,34402,24376,18750,13010,20403,20000,34000,29000,31700,33400,22965,22900,20700,13300,39349,39349,3215,3215,9665,9465,16000,17000,26000,27000,20200,16700,37730,28900,31900,32100,22400,22500,18500,15000,34520,30400,40025,32154,28463,21529,36000,36000,24707,24516,37900,27550,29000,23500,46293,32207,29000,22945,20700,20700,30535,30535,3215,3215,9465,9465,39560,39560,22466,22466,19550,18800,32207,41664,25048,19587,19638,15169,12600,10600,20200,15944,20700,20700,33803,33803,20200,20200,34100,34100,13000,11500,20000,20000,19000,16000,47450,35400,19273,19273,41400,41400,39100,39100,26141,26141,17700,16750,30300,28500,19273,19273,36000,36000,22200,20200,40000,30390,31350,29100,20000,16800,19547,19547,35785,35785,20700,20700,39346,39346,19500,19500,34800,35000,34000,34000,26141,26141,20405,20405,39100,39100,19550,19550,35791,35791,19550,19550,35791,35791,35700,35700,24650,24650,34600,36700,24700,25600,27607,25000,18588,14231,18800,18500,25400,25200,19388,25235,21337,24288,19273,19273,41400,41400,19272,19272,34000,34000,21100,15400,14000,10500,18900,19300,31822,32650,18000,18000,41001,39001,20000,9800,40900,29200,11400,7982,26554,22366,34000,34000,26100,26100,36400,25300,23000,19000,39000,51000,23000,28000,31700,35850,22300,22600,32900,33120,28000,27100,36100,33100,24000,24000,22000,22864,34200,27150,19100,19100,30500,30500,41664,41666,24650,24650,19600,19400,33800,31900,20000,19100,39930,37650,20000,15260,14445,10730,15500,17000,30300,34300,28500,28000,21500,19500,31300,32550,25300,26000,11500,11225,19300,15805,46400,32300,31000,22000,36000,36000,24691,24691,19547,19547,41667,41667,29200,27800,18600,15300,19550,19550,41664,41664,32300,33300,23000,28000,39349,39349,26100,26100,20500,30700,28250,25900,40349,39349,26100,26500,36000,36000,24689,24689,46293,32207,29000,22945,20500,19500,39000,34400,20700,20700,33803,33803,19550,19550,35791,35791,35791,35791,24650,24650,18000,15300,28100,30500,41500,36000,20800,24500,20700,20700,39346,39346,39001,39001,26000,26000,11500,9375,19351,14974,19600,19600,41400,41400,12400,7200,16500,9300,33803,34203,26100,26100,20407,15907,39100,39100,33500,31650,25500,24300,41664,41664,24650,24450,22050,20900,36300,32550,34000,34000,26141,26141,19270,19200,34000,34000,19272,19072,36000,36000,34300,28050,21000,16880,33800,33800,26100,26100,42001,37496,25500,24000,35500,35560,24689,24626,19458,19458,36000,34000,24150,18000,17700,17100,36000,36000,24689,24689,20406,20412,34000,34000,16400,18600,34649,38649,21200,19400,35700,31200,19269,19272,41400,41400,3715,19458,13793,32207,19200,20300,34030,38080,20700,21000,33803,33000,18250,17000,29575,22660,25600,29200,19000,21000,19272,19272,36000,36000,20400,20401,34000,34000,19272,19272,36000,36000,23400,23400,18216,18216,32650,31000,23000,25000,40200,40200,24650,24650,20700,20700,33802,33802,39100,39100,26141,26141,20063,19608,39010,39010,41662,28984,24650,19504,19272,19200,34000,34000,38900,39900,26000,26000,36000,36000,25000,25000,42501,33001,22000,22800,22482,22482,39330,39330,19269,19269,41400,41400,20400,20400,41410,50410,34003,33803,26100,26100,41400,41400,24689,24689,30500,15300,24045,19407,4042,19458,14086,32207,22333,22333,39340,39340,20700,20700,33700,33700,30000,14700,24000,23000,19000,19000,32000,35000,20000,20000,34000,34000,19272,19272,40400,41400,19500,18800,33020,34920,39259,37430,24000,22500,20100,20100,39101,39101,41664,41564,24650,24650,14000,12350,25300,22200,12793,32207,6601,8003,30000,36000,24782,24532,39100,38900,26141,26141,39100,39100,26141,26141,32300,30600,22800,22400,18500,18800,39100,33150,22450,19289,38050,31337,34525,37350,21400,23100,20700,20700,34306,33803,45893,31211,28340,23034,30600,30400,24600,24400],[22673,22673,46000,46000,46000,46000,29046,29046,39768,39768,29000,29000,23000,23000,39768,39768,23000,23000,39768,39768,23000,23000,39768,39768,46293,46293,29000,29000,22673,22673,46000,46000,22673,22673,40000,40000,39768,39768,29000,29000,46000,46000,29046,29046,40000,40000,29046,29046,46000,46000,29046,29046,22673,22673,40000,40000,19516,19516,46000,46000,46000,46000,29046,29046,23000,23000,39768,39768,46000,46000,29046,29046,22673,22673,40000,40000,22673,22673,40000,40000,40000,40000,29046,29046,39768,39768,29000,29000,46293,46293,29000,29000,39768,39768,29000,29000,40000,40000,23794.257,23794.257,23000,23000,46293,46293,46001,46001,29046,29046,46293,46293,29000,29000,39768,39768,29000,29000,22673,22673,46000,46000,23000,23000,46293,46293,22673,22673,40000,40000,40000,40000,29046,29046,23000,23000,46293,46293,46000,46000,29046,29046,46293,46293,29000,29000,23000,23000,46293,46293,23000,23000,39768,39768,23000,23000,46293,46293,46000,46000,29046,29046,46000,46000,29046,29046,23000,23000,46293,46293,46000,46000,29046,29046,22673,22673,40000,40000,46293,46293,29000,29000,23000,23000,46293,46293,22673,22673,40000,40000,22673,22673,46000,46000,23000,23000,39768,39768,46293,46293,29000,29000,22673,22673,46000,46000,46000,46000,29046,29046,40000,40000,29046,29046,39768,39768,29000,29000,46293,46293,29000,29000,23000,23000,39768,39768,22673,22673,40000,40000,40000,40000,29046,29046,23000,23000,46293,46293,39768,39768,29000,29000,46293,46293,29000,29000,23000,23000,39768,39768,22673,22673,46000,46000,22673,22673,40000,40000,23000,23000,39768,39768,22673,22673,46000,46000,46000,46000,29046,29046,22673,22673,40000,40000,22673,22673,40000,40000,22673,22673,40000,40000,46293,46293,29000,29000,23000,23000,39768,39768,23000,23000,46293,46293,23000,23000,39768,39768,40000,40000,29046,29046,22673,22673,46000,46000,23000,23000,39768,39768,23000,23000,39768,39768,39768,39768,29000,29000,46000,46000,29046,29046,46000,46000,29046,29046,22673,22673,40000,40000,46293,46293,29000,29000,22673,22673,46000,46000,22673,22673,40000,40000,40000,40000,29046,29046,23000,23000,46293,46293,22673,22673,46000,46000,22673,22673,46000,46000,22673,22673,40000,40000,40000,40000,23794.257,23794.257,46293,46293,29000,29000,40000,40000,29046,29046,46293,46293,29000,29000,46293,46293,29000,29000,46000,46000,29046,29046,23000,23000,39768,39768,22673,22673,40000,40000,46293,46293,29000,29000,22673,22673,40000,40000,23000,23000,46293,46293,40000,40000,29046,29046,23000,23000,46293,46293,40000,40000,29046,29046,39768,39768,29000,29000,23000,23000,39768,39768,46293,46293,29000,29000,40000,40000,29046,29046,23000,23000,46293,46293,39768,39768,29000,29000,23000,23000,46293,46293,46000,46000,29046,29046,46293,46293,29000,29000,22673,22673,46000,46000,46293,46293,29000,29000,40000,40000,29046,29046,46293,46293,29000,29000,23000,23000,46293,46293,23000,23000,39768,39768,23000,23000,39768,39768,39768,39768,29000,29000,23000,23000,39768,39768,46293,46293,29000,29000,23000,23000,46293,46293,46000,46000,29046,29046,23000,23000,39768,39768,22673,22673,46000,46000,22673,22673,40000,40000,39768,39768,29000,29000,22673,22673,46000,46000,40000,40000,22933,22933,46293,46293,29000,29000,22673,22673,40000,40000,40000,40000,23794.257,23794.257,22673,22673,40000,40000,22673,22673,40000,40000,40000,40000,29046,29046,39768,39768,29000,29000,46000,46000,29046,29046,40000,40000,29046,29046,22673,22673,40000,40000,46000,46000,29046,29046,40000,40000,29046,29046,22673,22673,40000,40000,23000,23000,46293,46293,22673,22673,40000,40000,22673,22673,46000,46000,22673,22673,46000,46000,23000,23000,46293,46293,23000,23000,39768,39768,23000,23000,39768,39768,39768,39768,29000,29000,22673,22673,40000,40000,22673,22673,40000,40000,22673,22673,40000,40000,40000,40000,23794.257,23794.257,40000,40000,29046,29046,46293,46293,29000,29000,23000,23000,39768,39768,46000,46000,29046,29046,22673,22673,46000,46000,46293,46293,29000,29000,22673,22673,40000,40000,46293,46293,29000,29000,40000,40000,29046,29046,46000,46000,29046,29046,23000,23000,39768,39768,22673,22673,46000,46000,22673,22673,46000,46000,39768,39768,29000,29000,46000,46000,29046,29046,40000,40000,29046,29046,23000,23000,46293,46293,22673,22673,40000,40000,23000,23000,39768,39768,40000,40000,29046,29046,22673,22673,46000,46000,22673,22673,40000,40000,22673,22673,46000,46000,23000,23000,39768,39768,46293,46293,29000,29000,22673,22673,46001,46001,46293,46293,29000,29000,39768,39768,29000,29000,46000,46000,29046,29046,40000,40000,29046,29046,46000,46000,29046,29046,46000,46000,29046,29046,39768,39768,29000,29000,23000,23000,46293,46293,22673,22673,40000,40000,46293,46293,29000,29000,23000,23000,39768,39768,46293,46293,29000,29000,40000,40000,29046,29046],[0.858,0.858,0.7,0.7,-0,-0.08699999999999999,0.07000000000000001,0.07000000000000001,0.15,0.15,0.1,0.1,0.261,0.261,0.288,0.288,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.25,0.136,0.15,0.116,0.197,0.118,0.152,0.217,0.153,0.153,0.25,0.25,0.339,0.346,0.328,0.334,0.1,0.1,0.116,0.17,0.162,0.248,0.38,0.38,0.104,0.104,0.15,0.15,0.206,0.422,-0.032,0.456,0.026,-0.199,0.609,0.457,0.13,0.13,0.15,0.15,0.846,0.783,0.496,0.246,-0,0.301,0.002,0.21,0.038,-0.063,0.609,0.5659999999999999,0.153,0.153,0.15,0.15,0.12,0.095,0.157,0.191,0.21,0.311,0.303,0.431,0.15,0.15,0.1,0.1,0.234,0.277,0.017,0.07199999999999999,0.15,0.15,-0.08500000000000001,-0.143,0.126,0.117,0.115,0.112,0.326,0.363,0.243,0.277,0.038,0.193,0,0.23,0.062,0.028,0.121,0.15,0.104,0.151,0.1,0.1,0.194,0.234,0.064,0.363,0.036,0.263,0.25,0.292,0.308,0.332,0.391,0.391,0.1,0.1,0.15,0.15,0.148,0.148,0.115,0.101,0.675,0.848,0.622,0.827,0.161,0.152,0.114,0.223,0.13,0.167,0.174,0.275,0.389,0.15,0.1,0.599,0.552,0.784,0.5590000000000001,0.786,0.1,0.1,0.15,0.15,0.426,0.578,0.795,0.877,0.252,0.47,0.354,0.552,0.1,0.118,0.15,0.275,0.315,0.279,0.208,0.21,0.1,0.422,0.15,0.15,0.858,0.858,0.758,0.763,0.294,0.25,0.435,0.413,0.122,0.274,0.051,0.273,0.311,0.307,0.228,0.224,0.184,0.338,0.25,0.339,0.13,0.301,0.02,0.259,0.1,0.1,0.149,0.156,0.047,0.307,0,0.19,0,0.304,0,0.209,0.1,0.1,0.232,0.232,0.858,0.858,0.763,0.763,0.011,0.011,0.227,0.227,0.15,0.183,0.304,0.1,0.37,0.507,0.323,0.477,0.728,0.771,0.303,0.45,0.1,0.1,0.15,0.15,0.109,0.109,0.259,0.259,0.427,0.493,0.5,0.5,0.174,0.304,-0.193,0.11,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.219,0.261,0.242,0.288,0.15,0.15,0.1,0.1,0.021,0.109,0,0.24,0.323,0.371,0.31,0.421,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.152,0.152,0.125,0.12,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.15,0.15,0.1,0.1,0.102,0.102,0.15,0.15,0.248,0.202,0.15,0.119,0.4,0.457,0.36,0.51,0.171,0.184,0.365,0.37,0.581,0.455,0.264,0.162,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.472,0.615,0.518,0.639,0.178,0.161,0.313,0.295,0.206,0.206,0.109,0.152,0.118,0.5679999999999999,0.111,0.365,0.497,0.648,0.336,0.441,0.15,0.15,-0.097,-0.097,0.214,0.453,0.207,0.345,0.025,-0.275,0.208,0.036,0.315,0.226,0.231,0.221,0.289,0.285,0.034,0.066,0.215,0.28,0.174,0.174,0.043,0.006,0.14,0.317,0.158,0.158,0.238,0.238,0.1,0.1,0.15,0.15,0.136,0.144,0.155,0.202,0.13,0.17,0.137,0.187,0.5,0.619,0.503,0.631,0.326,0.261,0.345,0.259,0.288,0.3,0.26,0.329,0.213,0.182,0.128,0.103,0.5,0.512,0.515,0.603,-0.002,0.302,-0.06900000000000001,0.241,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.266,0.301,0.359,0.472,0.15,0.15,0.1,0.1,0.298,0.276,0.208,0.036,0.15,0.15,0.1,0.1,0.096,-0.354,0.386,0.437,0.128,0.15,0.1,0.08599999999999999,0.1,0.1,0.15,0.15,0,0.304,0,0.209,0.109,0.152,0.158,0.257,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.217,0.335,0.293,0.233,0.104,0.222,0.283,0.155,0.1,0.1,0.15,0.15,0.152,0.152,0.105,0.105,0.5,0.592,0.513,0.623,0.136,0.136,0.1,0.1,0.453,0.6820000000000001,0.588,0.767,0.15,0.14,0.1,0.1,0.1,0.298,0.15,0.15,0.162,0.209,-0.112,-0.06,0.1,0.1,0.15,0.157,0.027,0.078,0.092,0.186,0.15,0.15,-0.099,-0.099,0.15,0.153,0.15,0.15,0.15,0.159,0.1,0.1,0.142,0.299,0.277,0.419,0.15,0.15,0.1,0.1,0.08699999999999999,0.185,0.122,0.174,0.112,0.111,0.15,0.152,0.142,0.142,0.1,0.15,0.475,0.609,0.391,0.411,0.1,0.1,0.15,0.15,0.1,0.1,0.15,0.15,0.287,0.191,0.252,0.165,0.065,0.144,0.108,0.22,0.15,0.15,0.1,0.1,0.836,0.142,0.7,0.3,0.165,0.117,0.265,0.177,0.1,0.08699999999999999,0.15,0.17,0.207,0.261,0.256,0.43,0.356,0.266,0.345,0.276,0.15,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.415,0.415,0.234,0.234,0.184,0.225,0.208,0.139,0.132,0.132,0.15,0.15,0.1,0.1,0.15,0.15,0.15,0.15,0.1,0.1,0.115,0.135,0.152,0.152,0.1,0.374,0.15,0.327,0.15,0.153,0.15,0.15,0.16,0.138,0.103,0.103,0.1,0.1,0.139,0.139,0.076,0.283,0.243,0.215,0.023,0.023,0.011,0.011,0.15,0.15,0.1,0.1,0.1,0.1,0.1,-0.096,0.145,0.15,0.1,0.1,0.1,0.1,0.15,0.15,0.238,0.618,0.172,0.332,0.824,0.154,0.696,0.304,0.015,0.015,0.016,0.016,0.1,0.1,0.153,0.153,0.25,0.632,0.174,0.208,0.162,0.162,0.304,0.239,0.118,0.118,0.15,0.15,0.15,0.15,0.122,0.1,0.152,0.183,0.17,0.122,0.152,0.191,0.172,0.224,0.113,0.113,0.15,0.15,0.1,0.102,0.15,0.15,0.648,0.6889999999999999,0.128,0.234,0.722,0.3,0.773,0.724,0.25,0.1,0.147,0.155,0.15,0.154,0.1,0.1,0.15,0.15,0.1,0.1,0.188,0.231,0.214,0.228,0.196,0.183,0.155,0.284,0.01,0.149,0.049,0.217,0.254,0.193,0.262,0.203,0.1,0.1,0.137,0.15,0.008999999999999999,0.326,0.023,0.206,0.235,0.24,0.153,0.16],[-3248.6,-3248.6,-3682.8,-3682.8,2758.8,3558.8,811,811,653.6,653.6,631,631,-491.6,-491.6,-447,-447,18.40000000000002,18.39999999999995,1051.2,1051.2,248.4,248.4,653.6,653.6,498.5999999999997,1558.6,341,541,-251.6,108.4,1358.6,758.6,-51.60000000000002,-51.60000000000002,-107,-107,-847,-907,-689,-729,1838.6,1838.6,548.8,233.6,593,-87,-989,-989,1798.6,1798.6,349.4,349.4,-291.6,-1271.6,2153,-1757,-91.60000000000004,788.3999999999999,-2841.400000000001,-1441.4,1558.6,1558.6,348.8,348.8,-3183.2,-2891.6,-2100,-107,2759,-14.4,1211,1,469,928,-2980,-2634.599999999999,-51.59999999999998,-51.60000000000002,693,693,933,1133,311,111,173,-627,-549,-1289,1428.4,1428.4,631,631,-17,-357,1111,791,693,693,576.4,848.6,128.4,168.4,1748.6,1778.6,-241.2,-581.2,-189,-389,2464.6,1026.6,1211,-123,1353,1623,511,341,173.4,-41.6,1838.6,1838.6,-183,-366.3999999999999,2228,-540.2,478.4,-551.6,-107,-447,-567,-767,-1049,-1049,248.4,248.4,1428.4,1428.4,1398.6,1398.6,551,631,-3431.4,-5036.4,-2399,-3584,-31.59999999999998,8.399999999999995,1758.6,748.6,108.4,-57.59999999999999,466.6,-338.4,-1081.5,17.39999999999999,1891.8,-2728.3752,-2319.4,-4450.4,-2028.8,-3345,1838.6,1838.6,348.8,348.8000000000001,-1251.6,-1951.6,-4541.4,-5305.4,439,-1566.2,-839,-1987,189,108.4,693,-307,-101.4,238.6,4,-9,248.4,-1231.6,1428.4,1428.4,-3248.6,-3248.6,-4174,-4214,-691.6,-491.6,-1241.4,-1041.4,148.4,-551.6,1439,-327,-61.4,-21.4,-109,-89,-191.6,-891.6,462.6,-361.3999999999999,1563.6,-10.60000000000006,1103.6,-283.2,1093,1093,352.4,314.1999999999999,1473,-597,1211,111,2817.2,0,1211,0,248.4,248.4,0,0,-3248.6,-3248.6,-4214,-4214,1805,1805,-95.80000000000003,-95.80000000000003,18.4,-131.6,0,1891.4,-1097.4,-2189.6,-661.4,-1555.2,-3921.4,-4321.4,-549,-1400.2,248.4,248.4,653.6,653.6,148.4,148.3999999999999,378.6,378.6000000000001,-1291.6,-1591.6,-2107,-2107,-91.60000000000008,-691.6,3383,973,-37,-37,1838.6,1838.6,1378.6,1378.6,639.2,639.2,-351.6,-541.6,-47,-407,-37,-37,1093,1093,548.4,148.4,1893,-29,-171.3999999999999,-621.4,-589,-1229,17.8,17.79999999999999,1050,1050,248.4,248.4,1427.8,1427.8,8.399999999999995,8.4,853,893,693,693,639.2,639.2,189.4,189.4,1378.6,1378.6,18.40000000000001,18.40000000000002,1051.2,1051.2,18.4,18.39999999999999,1051.2,1051.2,1033,1033,341,341,478.6,898.6000000000001,351,531,-920,-1441.4,-871.3999999999999,-1742.8,-131.6,-191.6,-1027,-1067,-2563.8,-1394.4,-321.6000000000001,268.6000000000001,-37,-37,1838.6,1838.6,-37.2,-37.19999999999998,693,693,-1887,-3027,-1789,-2489,-111.6,-31.6,-77,88.60000000000008,-291.6,-291.6,1758.8,1358.8,108.4,-1931.6,1738.6,-601.4,-1611.6,-2295.2,-796.2000000000002,-1633.8,693,693,631,631,838.6,-1381.4,11,-789,1693,4093,11,1011,-101.4,728.6,-129,-69,138.6,182.6,1011,831,778.6000000000001,178.6,211,211,508.4,681.2,733,-677,-71.59999999999999,-71.59999999999999,-7,-7,1891.4,1891.8,341,341,28.40000000000001,-11.59999999999999,653,273,108.4,-71.59999999999999,1544.6,1088.6,-2107,-3055,-1700,-2443,-791.6,-491.6,-381.4,418.6,-407,-507,-289,-689,153,403,471,611,-1591.6,-1646.6,-2247,-2946,2838.6,18.6,1611,-189,1093,1093,349.1999999999999,349.1999999999999,17.8,17.80000000000002,1892,1892,-267,-547,-869,-1529,18.39999999999999,18.40000000000001,1891.4,1891.4,18.59999999999998,218.6000000000001,11,1011,1428.4,1428.4,631,631,208.4,2248.4,-791.4,-1261.4,1628.4,1428.4,631,711,1093,1093,348.8000000000001,348.8000000000001,2817.2,0,1211,0,208.4,8.400000000000006,1358.6,438.6000000000001,248.4,248.4,653.6,653.6,18.39999999999991,18.39999999999995,1051.2,1051.2,1051.2,1051.2,341,341,-291.6,-831.6,-487,-7,1858.6,758.6,-429,311,248.4,248.4,1427.8,1427.8,1358.8,1358.8,611,611,-1591.6,-2016.6,-2236.8,-3112.2,28.40000000000001,28.40000000000002,1838.6,1838.6,-1411.6,-2451.6,-2807,-4247,653.6,733.6,631,631,189.8000000000001,-710.2000000000002,1378.6,1378.6,593,223,511,271,1891.4,1891.4,341,301,518.4,288.4,1153,403,693,693,639.2,639.2,-37.59999999999998,-51.59999999999998,693,693,-37.20000000000002,-77.20000000000002,1093,1093,753,-497,-389,-1213,653,653,631,631,1958.8,1057.8,511,211,993,1005,348.8000000000001,336.2,0,0,1093,693,-1611.4,-2841.4,-1049,-1169,1093,1093,348.8,348.8000000000001,189.6,190.8,693,693,-611.6000000000001,-171.6000000000001,488.4,1288.4,348.4,-11.6,null,null,-37.80000000000001,-37.2,1838.6,1838.6,-3148.6,0,-3682.8,0,-51.60000000000004,168.4,364.6,1174.6,248.4,308.3999999999999,653.6,493,-241.6,-491.6,-192,-1575,-987,-267,-789,-389,-37.19999999999998,-37.19999999999999,1093,1093,188.4,188.6,693,693,-37.20000000000002,-37.20000000000001,1093,1093,-1427,-1427,-945.8,-945.8,423,93,11,411,1598.6,1598.6,341,341,248.4,248.4,653.4,653.4,1378.6,1378.6,639.2,639.2,121,30,1360.6,1360.6,1891,-644.6,341,-688.2,-37.2,-51.59999999999999,693,693,1338.6,1538.6,611,611,1093,1093,411,411,2058.8,158.8,-189,-29,604.8,604.8,1759,1759,-37.79999999999998,-37.80000000000001,1838.6,1838.6,188.4,188.4,1840.6,3640.6,693.6,653.6,631,631,1838.6,1838.6,348.8,348.8,-7,-3047,220,-707.6,-3083.2,0,-3624.2,0,575,575,1761,1761,248.4,248.4,633,633,-107,-3167,211,11,-91.59999999999999,-91.59999999999999,-41.4,558.6,108.4,108.4,693,693,-37.2,-37.20000000000002,1638.6,1838.6,8.400000000000023,-131.6,497,877,1410.4,1044.6,211,-89,128.4,128.4,1378.8,1378.8,1891.4,1871.4,341,341,-3307,-3637,471,-149,-3882.8,0,-3268.8,-2988.4,-107,1093,367.4,317.3999999999999,1378.6,1338.6,639.2,639.2,1378.6,1378.6,639.2,639.2,353,13,-29,-109,-191.6,-131.6000000000001,1378.6,188.6,598.4,-33.8,1503,160.4,463.6,1028.6,-309,31,248.4,248.4,754.2,653.6,2737.2,-199.2,1079,17.8,13,-27,331,291],[false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,true,true,true,true,false,false,true,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,false,true,false,false,false,false,false,true,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,false,false,true,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,true,true,false,false,true,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,false,true,true,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,true,false,true,true,true,false,true,true,false,false,false,false,true,true,true,false,false,false,false,false,true,true,false,false,false,false,true,true,true,false,true,true,false,false,false,false,true,true,true,true,false,false,false,false,false,false,true,false,false,false,false,true,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,false,true,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,false,false,false,false,true,false,true,false,false,false,true,true,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,true,false,false,true,true,true,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,true,false,false,false,false,false,false,false,false,false,false,true,true,true,false,true,true,false,false,false,false,false,false,false,false,false,true,false,false,true,false,true,true,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,true,false,false,false,false,false,false,false,false],[false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,true,true,true,true,false,false,true,false,false,true,true,false,false,false,false,false,false,false,false,false,true,true,false,true,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,false,false,false,false,false,true,true,true,true,false,false,false,false,true,true,false,false,true,false,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,false,false,false,true,false,false,true,true,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,false,false,false,false,false,true,false,true,true,false,false,false,false,false,false,false,false,true,false,false,false,false,false,false,false,false,false,false,false,true,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,false,false,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,false,true,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,true,false,false,false,false,false,true,false,false,false,false,false,false,false,true,true,true,true,false,false,true,true,false,false,false,false,false,false,false,false,false,false,true,false,false,false,true,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,false,false,false,false,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,true,false,true,false,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,true,false,false,true,true,true,true,true,true,true,true,true,false,false,false,false,true,true,true,true,false,false,false,false,true,true,true,true,true,false,true,true,true,false,false,false,true,true,true,true,false,false,true,false,true,true,false,false,false,false,true,true,true,true,true,true,true,false,false,false,true,true,true,true,true,false,true,true,true,true,true,true,false,false,false,true,false,false,false,false,true,true,true,true,true,true,true,true,false,false,false,false,false,true,true,false,true,true,true,true,false,false,false,false,true,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,true,false,false,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,false,true,false,false,false,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,true,true,true,true,false,false,true,false,true,true,true,true,true,true,true,true,false,false,true,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,false,false,true,true,false,false,false,true,true,true,true,true,true,true,true,false,false,true,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,true,true,true,false,false,false,false,false,false,true,true]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>refClass<\/th>\n      <th>state<\/th>\n      <th>pct_goal<\/th>\n      <th>pct<\/th>\n      <th>rounded<\/th>\n      <th>block<\/th>\n      <th>plan<\/th>\n      <th>calc<\/th>\n      <th>edu<\/th>\n      <th>total_kWh<\/th>\n      <th>orig_kWh<\/th>\n      <th>pct_change<\/th>\n      <th>state_dif<\/th>\n      <th>matched_goal<\/th>\n      <th>matched_goal2<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"Blfrtip","buttons":["copy","csv","excel","pdf","print"],"pageLength":7,"autoWidth":true,"columnDefs":[{"className":"dt-right","targets":[1,5,7,11,12,13,14]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"id","targets":1},{"name":"refClass","targets":2},{"name":"state","targets":3},{"name":"pct_goal","targets":4},{"name":"pct","targets":5},{"name":"rounded","targets":6},{"name":"block","targets":7},{"name":"plan","targets":8},{"name":"calc","targets":9},{"name":"edu","targets":10},{"name":"total_kWh","targets":11},{"name":"orig_kWh","targets":12},{"name":"pct_change","targets":13},{"name":"state_dif","targets":14},{"name":"matched_goal","targets":15},{"name":"matched_goal2","targets":16}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[7,10,25,50,100]},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}</script>
<details class="code-fold">
<summary>Code</summary>

``` r
#DT::datatable(sum1 |> group_by(id,block) |> summarise(diff(total_kWh),diff(orig_kWh)))
```

</details>

## Looking at % of subjects who hit the target reduction

<details class="code-fold">
<summary>Code</summary>

``` r
# 203309



# Step 2: Summarize the percentage of participants meeting the goal for each condition
summary_pct <- s2_agg |>
  group_by(refClass, rounded, pct_goal) |>
  summarise(
    matched_count = sum(matched_goal),
    total_count = n(),
    pct_matched = matched_count / total_count * 100,
    .groups = "drop"
  )

# Step 3: Create plots for the percentage of subjects in each condition who matched the target pct
ggplot(summary_pct, aes(x = refClass, y = pct_matched, fill = rounded)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ pct_goal) +
  labs(
    title = "Percentage of Participants Matching Target Pct by Condition",
    x = "Reference Class",
    y = "Percentage Matched (%)",
    fill = "Rounding"
  ) +
  theme_minimal()
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-9-1.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
summary_pct |>
  gt() |>
  tab_header(
    title = md("**Percentage of Participants Matching Target Pct by Condition**"),
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal"
  ) |>
  fmt_number(
    columns = c(pct_matched),
    decimals = 2
  ) |>
  cols_label(
    refClass = "Reference Class",
    rounded = "Rounding",
    pct_goal = "Pct Goal",
    matched_count = "Matched Count",
    total_count = "Total Count",
    pct_matched = "Percentage Matched (%)"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_spanner(
    label = "Participants",
    columns = c(matched_count, total_count, pct_matched)
  )
```

</details>
<div id="ordclxumha" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ordclxumha table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ordclxumha thead, #ordclxumha tbody, #ordclxumha tfoot, #ordclxumha tr, #ordclxumha td, #ordclxumha th {
  border-style: none;
}

#ordclxumha p {
  margin: 0;
  padding: 0;
}

#ordclxumha .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ordclxumha .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ordclxumha .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ordclxumha .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ordclxumha .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ordclxumha .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ordclxumha .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ordclxumha .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ordclxumha .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ordclxumha .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ordclxumha .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ordclxumha .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ordclxumha .gt_spanner_row {
  border-bottom-style: hidden;
}

#ordclxumha .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ordclxumha .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ordclxumha .gt_from_md > :first-child {
  margin-top: 0;
}

#ordclxumha .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ordclxumha .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ordclxumha .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ordclxumha .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ordclxumha .gt_row_group_first td {
  border-top-width: 2px;
}

#ordclxumha .gt_row_group_first th {
  border-top-width: 2px;
}

#ordclxumha .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ordclxumha .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ordclxumha .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ordclxumha .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ordclxumha .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ordclxumha .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ordclxumha .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ordclxumha .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ordclxumha .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ordclxumha .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ordclxumha .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ordclxumha .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ordclxumha .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ordclxumha .gt_left {
  text-align: left;
}

#ordclxumha .gt_center {
  text-align: center;
}

#ordclxumha .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ordclxumha .gt_font_normal {
  font-weight: normal;
}

#ordclxumha .gt_font_bold {
  font-weight: bold;
}

#ordclxumha .gt_font_italic {
  font-style: italic;
}

#ordclxumha .gt_super {
  font-size: 65%;
}

#ordclxumha .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ordclxumha .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ordclxumha .gt_indent_1 {
  text-indent: 5px;
}

#ordclxumha .gt_indent_2 {
  text-indent: 10px;
}

#ordclxumha .gt_indent_3 {
  text-indent: 15px;
}

#ordclxumha .gt_indent_4 {
  text-indent: 20px;
}

#ordclxumha .gt_indent_5 {
  text-indent: 25px;
}

#ordclxumha .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ordclxumha div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" style="width:100%;" data-quarto-postprocess="true" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<colgroup>
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr class="gt_heading">
<th colspan="6" class="gt_heading gt_title gt_font_normal"><strong>Percentage of Participants Matching Target Pct by Condition</strong></th>
</tr>
<tr class="gt_heading">
<th colspan="6" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border">Grouped by Reference Class, Rounding, and Pct Goal</th>
</tr>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="refClass" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Reference Class</th>
<th rowspan="2" id="rounded" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Rounding</th>
<th rowspan="2" id="pct_goal" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Pct Goal</th>
<th colspan="3" id="Participants" class="gt_center gt_columns_top_border gt_column_spanner_outer" data-quarto-table-cell-role="th" scope="colgroup"><div class="gt_column_spanner">
Participants
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="matched_count" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Matched Count</th>
<th id="total_count" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Total Count</th>
<th id="pct_matched" class="gt_col_heading gt_columns_bottom_border gt_center" data-quarto-table-cell-role="th" scope="col">Percentage Matched (%)</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_center" headers="refClass">Percentage</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">8</td>
<td class="gt_row gt_center" headers="total_count">84</td>
<td class="gt_row gt_center" headers="pct_matched">9.52</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">Percentage</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">19</td>
<td class="gt_row gt_center" headers="total_count">60</td>
<td class="gt_row gt_center" headers="pct_matched">31.67</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">Percentage</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">27</td>
<td class="gt_row gt_center" headers="total_count">60</td>
<td class="gt_row gt_center" headers="pct_matched">45.00</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">Percentage</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">28</td>
<td class="gt_row gt_center" headers="total_count">84</td>
<td class="gt_row gt_center" headers="pct_matched">33.33</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">USD</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">8</td>
<td class="gt_row gt_center" headers="total_count">52</td>
<td class="gt_row gt_center" headers="pct_matched">15.38</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">USD</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">14</td>
<td class="gt_row gt_center" headers="total_count">58</td>
<td class="gt_row gt_center" headers="pct_matched">24.14</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">USD</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">12</td>
<td class="gt_row gt_center" headers="total_count">58</td>
<td class="gt_row gt_center" headers="pct_matched">20.69</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">USD</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">9</td>
<td class="gt_row gt_center" headers="total_count">52</td>
<td class="gt_row gt_center" headers="pct_matched">17.31</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">kWh</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">26</td>
<td class="gt_row gt_center" headers="total_count">60</td>
<td class="gt_row gt_center" headers="pct_matched">43.33</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">kWh</td>
<td class="gt_row gt_center" headers="rounded">Not Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">29</td>
<td class="gt_row gt_center" headers="total_count">76</td>
<td class="gt_row gt_center" headers="pct_matched">38.16</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">kWh</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">10%</td>
<td class="gt_row gt_center" headers="matched_count">35</td>
<td class="gt_row gt_center" headers="total_count">76</td>
<td class="gt_row gt_center" headers="pct_matched">46.05</td>
</tr>
<tr>
<td class="gt_row gt_center" headers="refClass">kWh</td>
<td class="gt_row gt_center" headers="rounded">Rounded</td>
<td class="gt_row gt_center" headers="pct_goal">15%</td>
<td class="gt_row gt_center" headers="matched_count">30</td>
<td class="gt_row gt_center" headers="total_count">60</td>
<td class="gt_row gt_center" headers="pct_matched">50.00</td>
</tr>
</tbody>
</table>

</div>
<details class="code-fold">
<summary>Code</summary>

``` r
# Step 1: Calculate whether participants matched the target pct_goal within different tolerances
sum1a <- s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, refClass, state, pct_goal, pct, rounded, block, plan, calc, edu) |> 
  summarise(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(
    target_pct = as.numeric(sub("%", "", pct_goal)) / 100,
    match_level = case_when(
      abs(pct_change - target_pct) <= 0.001 ~ "Exact Match",
      abs(pct_change - target_pct) <= 0.05 ~ "Within 5%",
      abs(pct_change - target_pct) <= 0.10 ~ "Within 10%",
      abs(pct_change - target_pct) <= 0.20 ~ "Within 20%",
      TRUE ~ "Over 20%"
    ),
    match_level = factor(match_level, levels = c("Exact Match", "Within 5%", "Within 10%", "Within 20%", "Over 20%"))
  )

# Step 2: Summarize the percentage of participants in each match level for each condition
summary_pct <- sum1a |>
  group_by(refClass, rounded, pct_goal, match_level) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(refClass, rounded, pct_goal) |>
  mutate(
    total_count = sum(count),
    percentage = count / total_count * 100
  ) |>
  ungroup()

# Step 3: Create plot
ggplot(summary_pct, aes(x = interaction(refClass, rounded), y = percentage, fill = match_level)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ pct_goal, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Reference Class and Rounding",
    y = "Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-1.png" width="1152" />

<details class="code-fold">
<summary>Code</summary>

``` r
ggplot(summary_pct, aes(x = refClass, y = percentage, fill = match_level)) +
  geom_bar(stat = "identity",position=position_dodge()) +
  facet_wrap(rounded ~ pct_goal, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Reference Class and Rounding",
    y = "Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-2.png" width="1152" />

## Cumulative plot

**Click on plots to enlarge**

<details class="code-fold">
<summary>Code</summary>

``` r
# Step 1: Calculate match levels with cumulative groups
sum1c <- s2_long |>
  filter(appliance != "TOTAL") |>
  group_by(id, refClass, state, pct_goal, pct, rounded, block, plan, calc, edu) |>
  summarise(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(
    target_pct = as.numeric(sub("%", "", pct_goal)) / 100,
    match_level = case_when(
      abs(pct_change - target_pct) <= 0.001 ~ "Exact Match",
      abs(pct_change - target_pct) <= 0.05 ~ "Within 5%",
      abs(pct_change - target_pct) <= 0.10 ~ "Within 10%",
      abs(pct_change - target_pct) <= 0.20 ~ "Within 20%",
      TRUE ~ "Over 20%"
    ),
    match_level = factor(
      match_level, 
      levels = c("Exact Match", "Within 5%", "Within 10%", "Within 20%", "Over 20%")
    )
  )

# Step 2: Create cumulative groups for each match level
summary_pct <- sum1c |>
  group_by(refClass, rounded, pct_goal, match_level) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(refClass, rounded, pct_goal) |>
  mutate(
    total_count = sum(count),
    cumulative_count = cumsum(count),
    cumulative_percentage = cumulative_count / total_count * 100
  ) |>
  ungroup()

# Step 3: Create a plot for cumulative percentages
ggplot(summary_pct, aes(x = refClass, y = cumulative_percentage, fill = match_level)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(pct_goal~rounded, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cumulative Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Reference Class",
    y = "Cumulative Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png" width="1152" />

<details class="code-fold">
<summary>Code</summary>

``` r
ggplot(summary_pct, aes(x = refClass, y = cumulative_percentage, fill = match_level)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~rounded, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cumulative Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Reference Class",
    y = "Cumulative Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-2.png" width="1152" />

<details class="code-fold">
<summary>Code</summary>

``` r
ggplot(summary_pct, aes(x = refClass, y = cumulative_percentage, fill = match_level)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cumulative Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Reference Class",
    y = "Cumulative Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-3.png" width="1152" />

<details class="code-fold">
<summary>Code</summary>

``` r
ggplot(summary_pct, aes(x = rounded, y = cumulative_percentage, fill = match_level)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cumulative Percentage of Participants by Match Level",
    subtitle = "Grouped by Reference Class, Rounding, and Pct Goal",
    x = "Rounding",
    y = "Cumulative Percentage (%)",
    fill = "Match Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-4.png" width="1152" />

## Main Effects and Interactions

**Click on plots to enlarge**

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = refClass, y = pct_change, fill = refClass)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Reference Class",
    x = "Reference Class",
    y = "% Change",
    fill = "Reference Class"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-1.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = pct_goal, y = pct_change, fill = pct_goal)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Goal % Reduction",
    x = "Goal % Reduction",
    y = "% Change",
    fill = "Goal % Reduction"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-2.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = rounded, y = pct_change, fill = rounded)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Rounding",
    x = "Rounding",
    y = "% Change",
    fill = "Rounding"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-3.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = pct_goal, y = pct_change, fill = refClass)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Goal % Reduction and Reference Class",
    x = "Goal % Reduction",
    y = "% Change",
    fill= "Reference Class"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-4.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = pct_goal, y = pct_change, fill = refClass)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  facet_wrap(~state) +
  labs(
    title = "Effect of Goal % Reduction and Reference Class - Separated by State",
    x = "Goal % Reduction",
    y = "% Change",
    fill= "Reference Class"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-5.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = pct_goal, y = pct_change, fill = rounded)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  facet_wrap(~rounded) +
  labs(
    title = "Effect of Goal % Reduction and Rounding",
    x = "Goal % Reduction",
    y = "% Change",
    fill= "Rounding"
  ) 
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-6.png" width="768" />

## Calculator and education

<details class="code-fold">
<summary>Code</summary>

``` r
# table with only education level counts, just use kable
s2 |> 
  group_by(edu) |> 
  summarise(n = n(), .groups = "drop") |>
  knitr::kable(caption = "Count of Items by Education Level", format = "html") 
```

</details>

| edu                       |   n |
|:--------------------------|----:|
| College degree            | 104 |
| Graduate degree           |  51 |
| Highschool diploma or GED |  11 |
| Some college              |  32 |
| Some graduate school      |   9 |

Count of Items by Education Level

<details class="code-fold">
<summary>Code</summary>

``` r
# table with only calculator counts

s2 |> 
  mutate(calc = if_else(calc == 0, "No Calculator", "Used Calculator")) |> 
  group_by(calc) |> 
  summarise(n = n(), .groups = "drop") |>
  knitr::kable(caption = "Count of Items by Calculator Use", format = "html")
```

</details>

| calc            |   n |
|:----------------|----:|
| No Calculator   |  39 |
| Used Calculator | 168 |

Count of Items by Calculator Use

<details class="code-fold">
<summary>Code</summary>

``` r
# s2 |> 
#   mutate(rounded = case_when(ROUNDED == 1 ~ "Not Rounded", ROUNDED == 2 ~ "Rounded")) |> 
#   group_by(calc, refClass, rounded) |> 
#   summarise(n = n(), .groups = "drop") |>
#   group_by(calc, refClass) |>
#   mutate(count_stats = as.character(n)) |>
#   ungroup() |>
#   tabulator(
#     rows = c("calc", "refClass"),
#     columns = "rounded",
#     `Count` = as_paragraph(count_stats)
#   ) |>
#   as_flextable() |> 
#   align(align = "left", part = "all") |>
#   autofit() |>
#   set_caption(
#     caption = "Count of Items by Calculation Type, Reference Class, and Rounded Value",
#     style = "Table Caption"
#   )


s2 |> 
  mutate(
    rounded = if_else(ROUNDED == 1, "Not Rounded", "Rounded"),
    calc = if_else(calc == 0, "No Calculator", "Used Calculator")
  ) |> 
  count(calc, refClass, rounded) |>
  pivot_wider(
    names_from = c(calc, rounded),
    values_from = n,
    values_fill = 0
  ) |>
  gt() |>
  tab_header(
    title = md("**Count of Items by Calculator Use, Reference Class, and Rounding**")
  ) |>
  cols_align(align = "left") |>
  tab_spanner_delim(delim = "_") |>
  cols_label(
    `No Calculator_Not Rounded` = "Not Rounded",
    `No Calculator_Rounded` = "Rounded",
    `Used Calculator_Not Rounded` = "Not Rounded",
    `Used Calculator_Rounded` = "Rounded"
  )
```

</details>
<div id="jxodsjowxn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jxodsjowxn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jxodsjowxn thead, #jxodsjowxn tbody, #jxodsjowxn tfoot, #jxodsjowxn tr, #jxodsjowxn td, #jxodsjowxn th {
  border-style: none;
}

#jxodsjowxn p {
  margin: 0;
  padding: 0;
}

#jxodsjowxn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jxodsjowxn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jxodsjowxn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jxodsjowxn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jxodsjowxn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jxodsjowxn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jxodsjowxn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jxodsjowxn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jxodsjowxn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jxodsjowxn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jxodsjowxn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jxodsjowxn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jxodsjowxn .gt_spanner_row {
  border-bottom-style: hidden;
}

#jxodsjowxn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#jxodsjowxn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jxodsjowxn .gt_from_md > :first-child {
  margin-top: 0;
}

#jxodsjowxn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jxodsjowxn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jxodsjowxn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#jxodsjowxn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#jxodsjowxn .gt_row_group_first td {
  border-top-width: 2px;
}

#jxodsjowxn .gt_row_group_first th {
  border-top-width: 2px;
}

#jxodsjowxn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jxodsjowxn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jxodsjowxn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jxodsjowxn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jxodsjowxn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jxodsjowxn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jxodsjowxn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jxodsjowxn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jxodsjowxn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jxodsjowxn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jxodsjowxn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jxodsjowxn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jxodsjowxn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jxodsjowxn .gt_left {
  text-align: left;
}

#jxodsjowxn .gt_center {
  text-align: center;
}

#jxodsjowxn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jxodsjowxn .gt_font_normal {
  font-weight: normal;
}

#jxodsjowxn .gt_font_bold {
  font-weight: bold;
}

#jxodsjowxn .gt_font_italic {
  font-style: italic;
}

#jxodsjowxn .gt_super {
  font-size: 65%;
}

#jxodsjowxn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jxodsjowxn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jxodsjowxn .gt_indent_1 {
  text-indent: 5px;
}

#jxodsjowxn .gt_indent_2 {
  text-indent: 10px;
}

#jxodsjowxn .gt_indent_3 {
  text-indent: 15px;
}

#jxodsjowxn .gt_indent_4 {
  text-indent: 20px;
}

#jxodsjowxn .gt_indent_5 {
  text-indent: 25px;
}

#jxodsjowxn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jxodsjowxn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="gt_heading">
<th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border"><strong>Count of Items by Calculator Use, Reference Class, and Rounding</strong></th>
</tr>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="refClass" class="gt_col_heading gt_columns_bottom_border gt_left" data-quarto-table-cell-role="th" scope="col">refClass</th>
<th colspan="2" id="spanner-No Calculator_Not Rounded" class="gt_center gt_columns_top_border gt_column_spanner_outer" data-quarto-table-cell-role="th" scope="colgroup"><div class="gt_column_spanner">
No Calculator
</div></th>
<th colspan="2" id="spanner-Used Calculator_Not Rounded" class="gt_center gt_columns_top_border gt_column_spanner_outer" data-quarto-table-cell-role="th" scope="colgroup"><div class="gt_column_spanner">
Used Calculator
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="No-Calculator_Not-Rounded" class="gt_col_heading gt_columns_bottom_border gt_left" data-quarto-table-cell-role="th" scope="col">Not Rounded</th>
<th id="No-Calculator_Rounded" class="gt_col_heading gt_columns_bottom_border gt_left" data-quarto-table-cell-role="th" scope="col">Rounded</th>
<th id="Used-Calculator_Not-Rounded" class="gt_col_heading gt_columns_bottom_border gt_left" data-quarto-table-cell-role="th" scope="col">Not Rounded</th>
<th id="Used-Calculator_Rounded" class="gt_col_heading gt_columns_bottom_border gt_left" data-quarto-table-cell-role="th" scope="col">Rounded</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left" headers="refClass">Percentage</td>
<td class="gt_row gt_left" headers="No Calculator_Not Rounded">7</td>
<td class="gt_row gt_left" headers="No Calculator_Rounded">5</td>
<td class="gt_row gt_left" headers="Used Calculator_Not Rounded">34</td>
<td class="gt_row gt_left" headers="Used Calculator_Rounded">30</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="refClass">USD</td>
<td class="gt_row gt_left" headers="No Calculator_Not Rounded">6</td>
<td class="gt_row gt_left" headers="No Calculator_Rounded">7</td>
<td class="gt_row gt_left" headers="Used Calculator_Not Rounded">27</td>
<td class="gt_row gt_left" headers="Used Calculator_Rounded">20</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="refClass">kWh</td>
<td class="gt_row gt_left" headers="No Calculator_Not Rounded">7</td>
<td class="gt_row gt_left" headers="No Calculator_Rounded">7</td>
<td class="gt_row gt_left" headers="Used Calculator_Not Rounded">24</td>
<td class="gt_row gt_left" headers="Used Calculator_Rounded">33</td>
</tr>
</tbody>
</table>

</div>
<details class="code-fold">
<summary>Code</summary>

``` r
# table with education level counts

s2 |>
  group_by(edu, refClass) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = refClass, values_from = n, values_fill = 0) |>
  gt(rowname_col = "edu") |>
  tab_header(title = md("**Count of Items by Education Level and Reference Class**")) |>
  cols_align(align = "left", columns = everything())
```

</details>
<div id="hmgjmqzjpp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hmgjmqzjpp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hmgjmqzjpp thead, #hmgjmqzjpp tbody, #hmgjmqzjpp tfoot, #hmgjmqzjpp tr, #hmgjmqzjpp td, #hmgjmqzjpp th {
  border-style: none;
}

#hmgjmqzjpp p {
  margin: 0;
  padding: 0;
}

#hmgjmqzjpp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hmgjmqzjpp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hmgjmqzjpp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hmgjmqzjpp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hmgjmqzjpp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmgjmqzjpp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmgjmqzjpp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmgjmqzjpp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hmgjmqzjpp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hmgjmqzjpp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hmgjmqzjpp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hmgjmqzjpp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hmgjmqzjpp .gt_spanner_row {
  border-bottom-style: hidden;
}

#hmgjmqzjpp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hmgjmqzjpp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hmgjmqzjpp .gt_from_md > :first-child {
  margin-top: 0;
}

#hmgjmqzjpp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hmgjmqzjpp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hmgjmqzjpp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hmgjmqzjpp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hmgjmqzjpp .gt_row_group_first td {
  border-top-width: 2px;
}

#hmgjmqzjpp .gt_row_group_first th {
  border-top-width: 2px;
}

#hmgjmqzjpp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmgjmqzjpp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hmgjmqzjpp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hmgjmqzjpp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmgjmqzjpp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmgjmqzjpp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hmgjmqzjpp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hmgjmqzjpp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hmgjmqzjpp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmgjmqzjpp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmgjmqzjpp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmgjmqzjpp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmgjmqzjpp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmgjmqzjpp .gt_left {
  text-align: left;
}

#hmgjmqzjpp .gt_center {
  text-align: center;
}

#hmgjmqzjpp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hmgjmqzjpp .gt_font_normal {
  font-weight: normal;
}

#hmgjmqzjpp .gt_font_bold {
  font-weight: bold;
}

#hmgjmqzjpp .gt_font_italic {
  font-style: italic;
}

#hmgjmqzjpp .gt_super {
  font-size: 65%;
}

#hmgjmqzjpp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hmgjmqzjpp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hmgjmqzjpp .gt_indent_1 {
  text-indent: 5px;
}

#hmgjmqzjpp .gt_indent_2 {
  text-indent: 10px;
}

#hmgjmqzjpp .gt_indent_3 {
  text-indent: 15px;
}

#hmgjmqzjpp .gt_indent_4 {
  text-indent: 20px;
}

#hmgjmqzjpp .gt_indent_5 {
  text-indent: 25px;
}

#hmgjmqzjpp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hmgjmqzjpp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| <strong>Count of Items by Education Level and Reference Class</strong> |  |  |  |
|----|----|----|----|
|  | Percentage | USD | kWh |
| College degree | 39 | 30 | 35 |
| Graduate degree | 21 | 15 | 15 |
| Highschool diploma or GED | 6 | 1 | 4 |
| Some college | 9 | 8 | 15 |
| Some graduate school | 1 | 6 | 2 |

</div>
<details class="code-fold">
<summary>Code</summary>

``` r
# table with calculator and education level counts
s2 |>
  mutate(calc = if_else(calc == 0, "No Calculator", "Used Calculator")) |> 
  group_by(calc, edu) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = edu, values_from = n, values_fill = 0) |>
  gt(rowname_col = "calc") |>
  tab_header(title = md("**Count of Items by Calculator Use and Education Level**")) |>
  cols_align(align = "left", columns = everything())
```

</details>
<div id="ihkanvvyeq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ihkanvvyeq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ihkanvvyeq thead, #ihkanvvyeq tbody, #ihkanvvyeq tfoot, #ihkanvvyeq tr, #ihkanvvyeq td, #ihkanvvyeq th {
  border-style: none;
}

#ihkanvvyeq p {
  margin: 0;
  padding: 0;
}

#ihkanvvyeq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ihkanvvyeq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ihkanvvyeq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ihkanvvyeq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ihkanvvyeq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ihkanvvyeq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ihkanvvyeq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ihkanvvyeq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ihkanvvyeq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ihkanvvyeq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ihkanvvyeq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ihkanvvyeq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ihkanvvyeq .gt_spanner_row {
  border-bottom-style: hidden;
}

#ihkanvvyeq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ihkanvvyeq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ihkanvvyeq .gt_from_md > :first-child {
  margin-top: 0;
}

#ihkanvvyeq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ihkanvvyeq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ihkanvvyeq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ihkanvvyeq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ihkanvvyeq .gt_row_group_first td {
  border-top-width: 2px;
}

#ihkanvvyeq .gt_row_group_first th {
  border-top-width: 2px;
}

#ihkanvvyeq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ihkanvvyeq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ihkanvvyeq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ihkanvvyeq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ihkanvvyeq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ihkanvvyeq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ihkanvvyeq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ihkanvvyeq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ihkanvvyeq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ihkanvvyeq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ihkanvvyeq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ihkanvvyeq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ihkanvvyeq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ihkanvvyeq .gt_left {
  text-align: left;
}

#ihkanvvyeq .gt_center {
  text-align: center;
}

#ihkanvvyeq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ihkanvvyeq .gt_font_normal {
  font-weight: normal;
}

#ihkanvvyeq .gt_font_bold {
  font-weight: bold;
}

#ihkanvvyeq .gt_font_italic {
  font-style: italic;
}

#ihkanvvyeq .gt_super {
  font-size: 65%;
}

#ihkanvvyeq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ihkanvvyeq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ihkanvvyeq .gt_indent_1 {
  text-indent: 5px;
}

#ihkanvvyeq .gt_indent_2 {
  text-indent: 10px;
}

#ihkanvvyeq .gt_indent_3 {
  text-indent: 15px;
}

#ihkanvvyeq .gt_indent_4 {
  text-indent: 20px;
}

#ihkanvvyeq .gt_indent_5 {
  text-indent: 25px;
}

#ihkanvvyeq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ihkanvvyeq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| <strong>Count of Items by Calculator Use and Education Level</strong> |  |  |  |  |  |
|----|----|----|----|----|----|
|  | College degree | Graduate degree | Highschool diploma or GED | Some college | Some graduate school |
| No Calculator | 21 | 11 | 1 | 4 | 2 |
| Used Calculator | 83 | 40 | 10 | 28 | 7 |

</div>
<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg |> ggplot(aes(x = calc, y = pct_change, fill = calc)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Calculator Use",
    x = "Calculator Use",
    y = "% Change",
    fill = "Calculator Use"
  )
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
## education

s2_agg |> ggplot(aes(x = edu, y = pct_change, fill = edu)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  labs(
    title = "Effect of Education",
    x = "Education",
    y = "% Change",
    fill = "Education"
  )
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-2.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
# sum1 |> 
#   ungroup() |> 
#   mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100,
#          block=factor(block)) |>
#   group_by(refClass,block,pct_goal) |>
#   summarise(mean_pct_change = mean(pct_change),se_pct_change = sd(pct_change)/sqrt(n())) |>
#   ggplot(aes(x=mean_pct_change,y=refClass,col=block)) + 
#   geom_pointrange(aes(xmin=mean_pct_change-se_pct_change,xmax=mean_pct_change+se_pct_change),position = position_dodge(width=0.5)) +
#   ggforce::facet_col(~pct_goal)
# 
# 
# sum1 |> 
#   ungroup() |> 
#   mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100,
#          block=factor(block)) |>
#   group_by(refClass,,pct_goal,state) |>
#   summarise(mean_pct_change = mean(pct_change),se_pct_change = sd(pct_change)/sqrt(n())) |>
#   ggplot(aes(x=mean_pct_change,y=refClass,col=pct_goal)) + 
#   geom_pointrange(aes(xmin=mean_pct_change-se_pct_change,xmax=mean_pct_change+se_pct_change),position = position_dodge(width=0.5)) +
#   ggforce::facet_col(~state)
# 
# 
# sum1 |> 
#   ungroup() |> 
#   mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100,
#          block=factor(block)) |>
#   group_by(refClass,,pct_goal,state,goal_pct) |>
#   summarise(mean_pct_change = mean(pct_change),se_pct_change = sd(pct_change)/sqrt(n())) |>
#   ggplot(aes(x=mean_pct_change,y=refClass,col=state)) + 
#   geom_vline(aes(xintercept=goal_pct),linetype="dashed",alpha=.5) +
#   geom_pointrange(aes(xmin=mean_pct_change-se_pct_change,xmax=mean_pct_change+se_pct_change),position = position_dodge(width=0.5)) +
#   ggforce::facet_col(~pct_goal)



  s2_long |> 
    filter(appliance != "TOTAL") |>
  group_by(refClass,appliance,state) |>
  summarise(mean_kWh_change = mean(change),se_pct_change = sd(change)/sqrt(n())) |>
  ggplot(aes(x=mean_kWh_change,y=refClass,col=appliance)) + 
  geom_pointrange(aes(xmin=mean_kWh_change-se_pct_change,xmax=mean_kWh_change+se_pct_change),position = position_dodge(width=0.5)) +
  ggforce::facet_col(~state)
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-14-1.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
s2_long |> 
  filter(appliance != "TOTAL") |>
  ggplot(aes(x=refClass,y=change,fill=state))+
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  scale_y_reverse() +
  facet_wrap(~appliance)
```

</details>

<img src="study2.markdown_strict_files/figure-markdown_strict/unnamed-chunk-14-2.png" width="768" />

<details class="code-fold">
<summary>Code</summary>

``` r
contingency_table <- sum1 %>%
    group_by(refClass) %>%
    summarise(
      matched = sum(matched_goal),
      unmatched = n() - sum(matched_goal)
    ) %>%
    select(matched, unmatched) %>%
    as.matrix()

# Perform the chi-squared test
chisq_test <- chisq.test(contingency_table)

# View the results
chisq_test


sum1b <- sum1 %>%
  mutate(matched_goal = as.numeric(matched_goal)) 

library(brms)
bayesian_model <- brms::brm(
  matched_goal ~ refClass,        # Logistic regression with reference class as predictor
  data = sum1,                    # The dataset
  family = bernoulli(),           # Binary outcome model
  prior = set_prior("normal(0, 1)", class = "b"),  # Prior for coefficients
  iter = 2000,                    # Number of iterations
  chains = 4,                     # Number of MCMC chains
  cores = 4,                      # Number of cores for parallel computation
  seed = 123                      # Set seed for reproducibility
)
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -1.27      0.14    -1.55    -1.00 1.00     3696     3393
# refClassPercentage    -0.56      0.22    -0.98    -0.14 1.00     3308     3065
# refClassUSD           -0.95      0.25    -1.45    -0.46 1.00     2895     2701


bm2 <- brms::brm(
  matched_goal ~ refClass + (1|id),        # Logistic regression with reference class as predictor
  data = sum1,                    # The dataset
  family = bernoulli(),           # Binary outcome model
  prior = set_prior("normal(0, 1)", class = "b"),  # Prior for coefficients
  iter = 2000,                    # Number of iterations
  chains = 4,                     # Number of MCMC chains
  cores = 4,                      # Number of cores for parallel computation
  seed = 123                      # Set seed for reproducibility
)
# Regression Coefficients:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             -1.92      0.31    -2.56    -1.37 1.00     2039     2669
# refClassPercentage    -0.73      0.36    -1.43    -0.02 1.00     2586     3142
# refClassUSD           -1.15      0.41    -1.95    -0.37 1.00     3079     3071



bm3 <- brms::brm(
  matched_goal ~ refClass*rounded + (1|id),        # Logistic regression with reference class as predictor
  data = sum1,                    # The dataset
  family = bernoulli(),           # Binary outcome model
  prior = set_prior("normal(0, 1)", class = "b"),  # Prior for coefficients
  iter = 2000,                    # Number of iterations
  chains = 4,                     # Number of MCMC chains
  cores = 4,                      # Number of cores for parallel computation
  seed = 123                      # Set seed for reproducibility
)
# Regression Coefficients:
#                                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            -1.80      0.34    -2.49    -1.17 1.00     1896     2384
# refClassPercentage                   -1.48      0.45    -2.37    -0.61 1.00     3108     3324
# refClassUSD                          -1.54      0.47    -2.53    -0.66 1.00     3309     3226
# roundedRounded                       -0.59      0.30    -1.17     0.02 1.00     4310     2857
# refClassPercentage:roundedRounded     1.71      0.45     0.83     2.59 1.00     4076     3070
# refClassUSD:roundedRounded            1.00      0.50     0.02     1.96 1.00     4399     3144



bm4 <- brms::brm(
  matched_goal ~ refClass*rounded + (1|id) +(1|state),        # Logistic regression with reference class as predictor
  data = sum1,                    # The dataset
  family = bernoulli(),           # Binary outcome model
  prior = set_prior("normal(0, 1)", class = "b"),  # Prior for coefficients
  iter = 2000,                    # Number of iterations
  chains = 4,                     # Number of MCMC chains
  cores = 4,                      # Number of cores for parallel computation
  seed = 123                      # Set seed for reproducibility
)
# Regression Coefficients:
#                                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            -1.77      0.44    -2.62    -0.81 1.01      526      187
# refClassPercentage                   -1.49      0.44    -2.34    -0.62 1.00     1352     1519
# refClassUSD                          -1.53      0.48    -2.49    -0.62 1.00     1836     2504
# roundedRounded                       -0.58      0.29    -1.16    -0.03 1.00     2933     2938
# refClassPercentage:roundedRounded     1.72      0.45     0.85     2.59 1.00     1936     2501
# refClassUSD:roundedRounded            1.00      0.48     0.05     1.91 1.00     2460     2472
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
# mixed effects model testing effect of refClass, state, and pct_goal on pct_change
# marginal effects plots
library(ggeffects)


m1 <- lmer(pct_change ~ refClass  + (1+plan|id) + (1|state), data = s2_agg)
summary(m1)
ggpredict(m1, c("refClass")) |> plot()

m1 <- lmer(pct_change ~ pct_goal  + (1|id) + (1|state), data = s2_agg)
summary(m1)
ggpredict(m1, c("pct_goal")) |> plot()

m1 <- lmer(pct_change ~ pct_goal*rounded  + (1|id) + (1|state), data = s2_agg)
summary(m1)
ggpredict(m1, c("pct_goal","rounded")) |> plot()


m1 <- lmer(pct_change ~ refClass*pct_goal*rounded  + (1|id) + (1|state), data = s2_agg)
ggpredict(m1, c("refClass","pct_goal","rounded")) |> plot()

m1 <- lmer(pct_change ~ state*pct_goal*rounded  + (1|id), data = s2_agg)
summary(m1)
ggpredict(m1, c("state","pct_goal","rounded")) |> plot()


# m1 <- lmer(pct_change ~ refClass*pct_goal*rounded*state  + (1|id), data = sum1)
# ggpredict(m1, c("refClass","pct_goal","rounded","state")) |> plot()
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
s2_long |> 
  filter(appliance != "TOTAL") |>
  ggplot(aes(x=refClass,y=state_dif,fill=state))+
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  scale_y_reverse() +
  facet_wrap(~appliance)

s2_long |> 
  filter(appliance != "TOTAL") |>
  ggplot(aes(x=state_dif,y=value,col=state))+
  geom_point() +
  facet_wrap(~appliance)


sum1 |> 
  ggplot(aes(x=state_dif,y=pct_change,col=state)) +
  geom_point() 


# compute total kWh reduction, and total % reduction 
s2_long |> group_by(id,refClass,state,pct_goal,rounded,block,plan) |> 
  filter(appliance !="TOTAL") |> 
  summarise(total_kWh = sum(value),orig_kWh=sum(family), total_pct = sum(value)/sum(family)) |>
  ggplot(aes(x = pct_goal, y = total_pct, fill = refClass)) + 
  geom_boxplot(position = "dodge") +
  facet_wrap(~state)
  
s2_long |> group_by(id,refClass,state,pct_goal,rounded) |> 
  filter(appliance !="TOTAL") |> 
  summarise(total_kWh = sum(reduction), total_pct = sum(reduction)/sum(family)) |>
  ggplot(aes(x = pct_goal, y = total_pct, fill = refClass)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") 
  

s2_long |> group_by(id,refClass,state,pct_goal,rounded) |> 
  filter(appliance !="TOTAL") |> 
  summarise(total_kWh = sum(reduction), orig_kWh=sum(family), total_pct = round(sum(reduction)/sum(family))) |>
  ggplot(aes(x = state1, y = total_pct, fill = refClass)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  facet_wrap(~pct_goal)
  

s2_long |> group_by(id,refClass,state,pct_goal,rounded,plan) |> 
  filter(appliance !="TOTAL") |> 
  summarise(total_kWh = sum(reduction), total_pct = sum(reduction)/sum(family)) |>
  ggplot(aes(x = refClass, y = total_pct, fill = pct_goal)) + 
  stat_summary(fun = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  facet_wrap(plan~rounded)

# sum1 |> 
#   ungroup() |> 
#   mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100) |>
#   mutate(id=reorder(id,pct_change)) |> 
#   ggplot(aes(y=id,x=pct_change)) + 
#   geom_point(size=1,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
#   geom_vline(aes(xintercept=goal_pct),linetype="dashed",alpha=.5) +
#   ggh4x::facet_nested_wrap(refClass~pct_goal,axes="all",scales="free",ncol=2) 

# sum1 |> 
#   ungroup() |> 
#   mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100) |>
#   mutate(id=reorder(id,pct_change)) |> 
#   ggplot(aes(y=id,x=pct_change,col=refClass)) + 
#   geom_point(size=1,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
#   geom_vline(aes(xintercept=goal_pct),linetype="dashed",alpha=.5) +
#   ggh4x::facet_nested_wrap(block~pct_goal,axes="all",scales="free",ncol=2) 
# 
# 
# sum1 |> 
#   ungroup() |> 
#     mutate(goal_pct = as.numeric(stringr::str_remove(pct_goal,"%"))/100) |>
#   ggplot(aes(x=state,y=pct_change,col=state)) + 
#   geom_point(size=1,alpha=0.6,position = position_jitter(w=0, h=0.17)) +
#     geom_hline(aes(yintercept=goal_pct),linetype="dashed",alpha=.5) +
#   ggdist::stat_halfeye()+
#   ggh4x::facet_nested_wrap(block~pct_goal,axes="all",ncol=2) 


# s2_long <- s2 |>
#   pivot_longer(
#     cols = c(ACFAMILY1:TOTALAPTWO1),
#     names_to = c(".value", "appliance"),
#     names_pattern = "(.*)(FAMILY1|STATE1|APONE1|APTWO1)$",
#     values_to = "value"
#   )




# s2 |> pivot_longer(cols = contains("FAMILY"), names_to = "Family", values_to = "kWh") |> relocate(Family,kWh) |> 
#   pivot_longer(cols = contains("STATE1"), names_to = "State", values_to = "kWh") |> relocate(State)


# 
# s2 |> 
#   pivot_longer(cols=ACFAMILY1:TOTALAPTWO1, names_to = "appliance", values_to = "family") |> relocate(appliance,family)
```

</details>
