# Response Patterns


``` r
library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here, haven,tibble,ggplot2,gt,ggh4x,lme4,flextable,ggdist)

#https://arelbundock.com/posts/quarto_figures/index.html
knitr::opts_chunk$set(
    out.width = "75%", # enough room to breath
    fig.width = 6,     # reasonable size
    fig.asp = 0.618,   # golden ratio
    fig.align = "center" # mostly what I want
)

out2fig = function(out.width, out.width.default = 0.7, fig.width.default = 6) {
    fig.width.default * out.width / out.width.default 
}



s1 <- readRDS(here::here("data/s1_processed.rds")) |> filter(!(id %in% readRDS(here::here("data/s1_discrep_ids.rds"))))
s2_long <- readRDS(here::here("data/s2_processed.rds")) |> filter(!(id %in% readRDS(here::here("data/s2_discrep_ids.rds"))))



s1_agg <- s1 |> 
  filter(appliance !="Total kWh") |> 
  group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
  summarise(total_kWh = sum(value),orig_kWh=sum(family), 
            pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
            n_change = sum(value!=family),
            state_p_dif=mean(state_p_dif),
            state_f_dif=mean(state_f_dif),
            n_less_avg = sum(less_avg),
            duration=first(Duration__in_seconds_)) |> 
  mutate(matched_goal = (pct_change == pct_goal), 
             error = pct_change - pct_goal,
            abs_error = abs(error))
```

## Study 1 - comparison against state average

``` r
#| 
s2_long |> filter(id %in% unique(s2_long$id)[1:30]) |> 
  filter(appliance!="TOTAL",state=="California") |> 
  ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=plan)) +
  geom_point(aes(y=state_avg),color="red") +
  geom_point(aes(y=family),color="blue") +
  facet_wrap(~id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-2-1.png"
style="width:75.0%" data-fig-align="center" />

``` r
s2_long |> filter(id %in% unique(s2_long$id)[1:30]) |> 
  filter(appliance!="TOTAL",state=="California") |> 
  ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=plan)) +
  geom_point(aes(y=state_avg),color="red") +
  geom_point(aes(y=family),color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-1.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
mutate(total_kWh = sum(value),orig_kWh=sum(family), 
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL",state=="CAL",value<20000) |> 
  ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=matched_goal)) +
  geom_point(aes(y=state_avg),color="red") +
  geom_point(aes(y=family),color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-2.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
mutate(total_kWh = sum(value),orig_kWh=sum(family), 
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL",state=="CAL",value<20000) |> 
  ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=matched_goal)) +
  geom_point(aes(y=state_avg),color="red") +
  geom_point(aes(y=family),color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~matched_goal)
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-3.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
mutate(total_kWh = sum(value),orig_kWh=sum(family), 
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL",state=="CAL",value<20000) |> 
  ggplot(aes(x=appliance,y=value)) + 
  geom_jitter(aes(color=matched_goal)) +
geom_point(aes(y=state_avg),color="red",size=10) +
  geom_point(aes(y=family),color="blue",size=5) 
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-4.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
mutate(total_kWh = sum(value),orig_kWh=sum(family), 
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL",value<20000,family<25000) |> 
  ggplot(aes(x=appliance,y=value)) + 
  geom_jitter(aes(color=matched_goal)) +
 # gghalves::geom_half_violin(aes(fill=matched_goal)) +
  geom_boxplot(aes(fill=matched_goal),alpha=.5) +
geom_point(aes(y=state_avg),color="red",size=7,shape=8) +
  geom_point(aes(y=family),color="blue",size=7,shape=17) +
  facet_wrap(~state)
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-5.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal) |> 
mutate(total_kWh = sum(value),orig_kWh=sum(family), 
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL",value<20000,family<25000) |> 
  ggplot(aes(x=appliance,y=state_p_dif)) + 
  geom_jitter(aes(color=matched_goal)) +
  geom_point(aes(y=state_avg),color="red",size=10) +
  geom_point(aes(y=family),color="blue",size=5)
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-6.png"
style="width:75.0%" data-fig-align="center" />

``` r
s1 |> 
filter(appliance !="Total kWh") |> 
group_by(id,refClass,state,block,plan,calc,edu,pct_goal,appliance) |> 
summarize(total_kWh = sum(value),orig_kWh=sum(family), 
            adjust=orig_kWh-total_kWh,
        pct_change = round((orig_kWh-total_kWh)/orig_kWh,3), 
        n_change = sum(value!=family),
        state_p_dif=mean(state_p_dif),
        state_f_dif=mean(state_f_dif),
        n_less_avg = sum(less_avg),
        duration=first(Duration__in_seconds_)) |> 
mutate(matched_goal = (pct_change == pct_goal), 
            error = pct_change - pct_goal,
        abs_error = abs(error)) |> 
          filter(appliance!="TOTAL") |> 
  ggplot(aes(x=appliance,y=adjust)) + 
  geom_jitter(aes(color=matched_goal)) +
  facet_wrap(~state)
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-3-7.png"
style="width:75.0%" data-fig-align="center" />

## Study 2 - comparison against state average

``` r
s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, refClass, appliance, state, pct_goal, pct, rounded, block, plan, calc, edu) |> 
  mutate(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(matched_goal = (pct_change == pct),
  matched_goal2 = abs(pct_change-pct)<.03) |> 
    ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=matched_goal)) +
    geom_point(aes(y=state_avg),color="red") +
    geom_point(aes(y=family),color="blue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(state~matched_goal,scales="free_y")
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-4-1.png"
style="width:75.0%" data-fig-align="center" />

``` r
s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, refClass, appliance, state, pct_goal, pct, rounded, block, plan, calc, edu) |> 
  mutate(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(matched_goal = (pct_change == pct),
  matched_goal2 = abs(pct_change-pct)<.03) |> 
    ggplot(aes(x=appliance,y=value)) + geom_point(aes(shape=matched_goal)) +
    geom_point(aes(y=state_avg),color="red") +
    geom_point(aes(y=family),color="blue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(refClass~state,scales="free_y") +
    labs(title="Study 2 - plans vs. state averages and prior year values",subtitle="Blue points are prior year values, red points are state averages, and black points are current year values")
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-4-2.png"
style="width:75.0%" data-fig-align="center" />

``` r
library(ggplot2)
library(dplyr)
s2_long %>%
  filter(appliance != "TOTAL") %>%
  group_by(id, refClass, appliance, state, pct_goal, pct, rounded, block, plan, calc, edu) %>%
  mutate(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) %>%
  mutate(
    matched_goal = (pct_change == pct),
    matched_goal2 = abs(pct_change - pct) < 0.03
  ) %>%
  ggplot(aes(x = appliance)) +
  # Points for current plan, with conditional color and shape for matched goal
  geom_point(aes(
    y = value,
    color = ifelse(matched_goal2, "Matched Goal", "Current Plan"),
    shape = ifelse(matched_goal2, "Matched Goal", "Current Plan"),
    size = ifelse(matched_goal2, 5, 2),  # Larger size for matched goal
    alpha = ifelse(matched_goal2, 1, 0.6),  # Less transparency for matched goal
    stroke = ifelse(matched_goal2, 1.5, 0)  # Outline for matched goal points
  )) +
  # Points for state average (red) with color mapped for legend
  geom_point(aes(y = state_avg, color = "State Average"), shape = 17, size = 3) +
  # Points for family prior year (blue) with color mapped for legend
  geom_point(aes(y = family, color = "Prior Year (Family)"), shape = 15, size = 3) +
  # Custom colors for each legend entry
  scale_color_manual(
    values = c(
      "Matched Goal" = "#00FF00",  # Bright green for matched goal
      "Current Plan" = "black",
      "State Average" = "red",
      "Prior Year (Family)" = "blue"
    ),
    name = "Legend"
  ) +
  scale_shape_manual(
    values = c(
      "Matched Goal" = 8,  # Star shape for matched goal
      "Current Plan" = 16  # Default shape for current plan
    )
  ) +
  scale_size_continuous(range = c(2, 1)) +  # Use continuous scale for size
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(refClass ~ state, scales = "free_y") +
  labs(
    title = "Study 2 - Plans vs. State Averages and Prior Year Values",
    subtitle = "Blue points are prior year values, red points are state averages, black points are current plans, bright green star points indicate plans that match goal"
  )
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-4-3.png"
style="width:75.0%" data-fig-align="center" />

``` r
s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, refClass, appliance, state, pct_goal, pct, rounded, block, plan, calc, edu) |> 
  mutate(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(
    matched_goal = (pct_change == pct),
    matched_goal2 = abs(pct_change-pct) < .03
  ) |> 
  ggplot(aes(x = appliance)) + 
    # Non-matching plans (plotted first, in background)
    geom_point(data = . %>% filter(!matched_goal2),
               aes(y = value, color = "Plan (Not Matching)", shape = "Plan"), 
               size = 1, alpha = 0.4) +
    # Matching plans
    geom_point(data = . %>% filter(matched_goal2),
               aes(y = value, color = "Plan (Matching Goal)", shape = "Plan"), 
               size = 1.5) +
    # Prior year values
    geom_point(aes(y = family, color = "Prior Year", shape = "Prior Year"), 
               size = 2.5) +
    # State averages with star shape, plotted last and largest
    geom_point(aes(y = state_avg, color = "State Average", shape = "State Average"), 
               size = 2, stroke = 2) +
    scale_color_manual(
      name = "Values",
      values = c(
        "State Average" = "#FF0000",     # Bright red
        "Prior Year" = "#4169E1",        # Royal blue
        "Plan (Matching Goal)" = "#228B22",
        "Plan (Not Matching)" = "#CD5C5C"
      )
    ) +
    scale_shape_manual(
      name = "Type",
      values = c(
        "State Average" = 8,    # Star
        "Prior Year" = 17,      # Triangle
        "Plan" = 16            # Circle
      )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "lightgray", color = NA),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) +
    facet_wrap(refClass ~ state, scales = "free_y") +
    labs(
      title = "Study 2 - Plans vs. State Averages and Prior Year Values",
      x = "Appliance",
      y = "Value"
    )
```

<img src="indiv_files/figure-commonmark/unnamed-chunk-5-1.png"
style="width:75.0%" data-fig-align="center" />
