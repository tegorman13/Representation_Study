---
title: 'Planning to Save Energy: How Information Format Affects Accuracy'
aliases:
  - /manuscript.html
format:
  wordcount-html:
    wordcount-banner: true
    theme:
      - zephyr
      - ../assets/custom.scss
    toc: true
    shift-heading-level-by: 1
    title-block-banner: '#000D51'
    include-in-header:
      - text: |
          <style type="text/css">
          h2.hide {
              display: none;
          }
          </style>
  html:
    echo: true
    code-fold: show
    output-file: manuscript-code.html
  hugo-md:
    include: false
    echo: false
    html-math-method: mathjax
    output-file: manuscript_hugo.md
    suppress-bibliography: true
  gfm:
    echo: true
    output-file: manuscript_gfm.md
    suppress-bibliography: true
  hikmah-manuscript-pdf:
    output-file: rep_study.pdf
    mainfont: Linux Libertine O
    mainfontoptions:
      - Numbers=Proportional
      - Numbers=OldStyle
    mathfont: Libertinus Math
    knitr:
      opts_chunk:
        dev: cairo_pdf
  docx: default
format-links:
  - text: HTML
    href: manuscript.html
    icon: file-minus
  - text: HTML (with code)
    href: manuscript-code.html
    icon: file-plus
  - format: hikmah-manuscript-pdf
    text: PDF version
    icon: file-pdf
  - docx
  - gfm
date: 2024-12-07T00:00:00.000Z
abstract: >
  Effective communication of energy consumption information is crucial for
  promoting residential energy conservation. This study investigates how
  different numerical representations of energy reduction goals influence
  consumers' ability to create accurate conservation plans. Across two
  experiments, we examined the impact of presenting energy information in
  kilowatt-hours (kWh), percentages, or U.S. dollars (USD) on planning accuracy.
  Participants completed a simulated household planning task in which they
  allocated energy usage across multiple appliances, with the goal presented in
  either kilowatt-hours (kWh), percentages, or monetary costs. Results across
  both experiments showed that presenting reduction goals in absolute units
  (kWh) led to significantly greater accuracy compared to percentage-based or
  monetary formats. Furthermore, we found that higher energy literacy was
  associated with more accurate planning. These findings demonstrate that
  absolute units (kWh) are more effective for communicating energy-saving goals,
  and highlight the potential value of educational interventions to improve
  consumer energy literacy.
---


# Introduction

-   highlight the significant contribution of residential energy consumption to carbon emissions and the potential for substantial reductions. Make point about urgency of climate change? (IPCC (2014) and EIA (2012) ).

-   Electricity bills are a primary source of energy-use information for consumers and offer a promising avenue for enhancing communication about energy consumption (Fischer, 2008)

### Literature Review

Energy poverty continues to be a pervasive issue in the United States (Memmott et al., 2021). This challenge partly arises from difficulties in converting information across numerical formats, impeding the development of precise energy reduction plans Reimer et al. (2015). Prior research by Canfield et al. (2017) demonstrated that presenting energy information in tabular formats enhances comprehension relative to graphs.

The way numerical information is presented can significantly affect how individuals process and use that information (Reimer et al., 2015). The reference class problem highlights that numbers without clear reference points can lead to misinterpretation, as the meaning of a statistic depends on the category or class it refers to (Gigerenzer & Edwards, 2003; Reimer et al., 2015). Presenting energy information in absolute units (e.g., kWh) provides a clear reference class, potentially enhancing comprehension.

The concept of cognitive fit posits that performance improves when the information presentation format aligns with the task requirements (Vessey, 1991), and that such an alignment can reduce cognitive load and enhance accuracy in planning (Shah & Freedman, 2011). For instance, tables are generally more effective than graphs for conveying specific electricity usage data because they facilitate straightforward point reading (Canfield et al., 2017). However, the effectiveness of the format varies with the type of information and individual differences, such as energy literacy, which significantly impacts comprehension and conservation intent. Moreover, the unit in which numerical information is presented influences how decision-makers evaluate and choose between options, with default units increasing value sensitivity (Herberz et al., 2020). In the context of energy, presenting information in terms of multiple translations can increase preference for options aligned with activated objectives, such as pro-environmental values (Ungemach et al., 2018). Furthermore, mental accounting mechanisms, where individuals create mental budgets linking specific consumption acts to specific payments, significantly impact energy decisions and behaviors (Hahnel et al., 2020).

Evidence from research on energy consumption feedback, normative comparisons, and eco-feedback platforms suggests that comprehensible and contextually meaningful data presentations can improve users' ability to plan reductions, especially when these formats are integrated into daily routines (Canfield et al., 2017; Fischer, 2008; Kim et al., 2022; Schwartz et al., 2015). Furthermore, temporal and monetary frames have been shown to alter decision quality, with monthly costs or absolute consumption levels often encouraging more energy-efficient intentions than abstract annual or percentage-based metrics (Gill et al., 2022; Larrick & Soll, 2008). In this context, tailoring reference classes to align with intuitive cognitive processes can help bridge the gap between aggregate reduction goals and targeted, appliance-specific conservation strategies.

Furthermore, research suggests that natural frequencies and absolute numbers are generally easier for individuals to understand compared to percentages or probabilities Hoffrage et al. (2000). In the context of energy conservation, using absolute units may facilitate more accurate planning and decision-making by aligning with intuitive cognitive processing.

Despite existing studies on energy-use communication and format effects, limited research has explored how different numerical representations influence consumers' ability to create accurate energy conservation plans. Specifically, there is a gap in understanding how presenting energy information in absolute units versus percentages or monetary terms affects the precision of planning appliance-specific reductions. Addressing this gap is crucial for developing effective interventions that promote energy conservation behaviors.

### Hypotheses

Building on these findings and informed by prior work showing that frequencies (like absolute units in kWh) are easier to comprehend and facilitate more precise decision-making compared to percentages, our study also utilizes a tabular format, but manipulates whether participants must consider energy information presented as absolute units (kWh), percentages (%), or monetary costs (USD). We hypothesize that presenting information in absolute units (kWh) will lead to more accurate household energy conservation planning.

# Experiment 1

See <a href="#fig-task" class="quarto-xref">Figure 1</a> for an example of a planning trial as it was seen by participants.

## Methods

### Participants

We implemented our task and surveys on Qualtrics, and recruited participants through Amazon Mechanical Turk. In Experiment 1, 252 participants were initially recruited, but data from 17 participants were corrupted due to experimenter error, leaving a final sample of 235 participants. Most participants (76%) reported using a calculator to complete the task.

### Materials and Design

The study employed a mixed design with reference class (kWh, percentage, USD) as a between-subjects factor and state/family scenario as a within-subjects factor. Each participant completed energy reduction planning tasks for two different states, with state order counterbalanced across participants. The family scenarios featured four households in different climate regions: Texas (Smith family) and California (Adams family) representing warm climates, and Colorado (Wells family) and Massachusetts (Davis family) representing cold climates. We obtain average utility use from each state by CITE SOURCE FOR STATE AVGS?

### Procedure

Participants received energy usage data for two hypothetical families and were tasked with creating action plans to meet specified reduction goals by allocating usage across five appliance categories: heating, cooling, water heating, refrigerator, and and other appliances (e.g., TV, lighting).

For each family scenario, the participants were shown a table containing the families utility usage from the prior year, alongside the state averages for each appliance category (both prior year usage and stage averages are always shown in kWh). For each scenario, participants were asked to create two possible action plans to achieve the target reduction in total household energy usage (see <a href="#fig-task" class="quarto-xref">Figure 1</a>). Depending on their reference class condition, the target reduction amount presented either in kilowatt-hours (kWh), as percentages of total household usage, or in U.S. dollars. In all conditions, the target reduction was equivalent to a 15% reduction in total household kWh.

<img src="./assets/images/Smith_10R_Wells_15E.png" id="fig-task"
alt="Figure 1: Example trial in the energy planning task. Participants are shown the prior year electricity use of a household, and are tasked with creating a plan for the next year that will meet the energy reduction goal. Study 1 manipulates the format of the reduction goal to be either a percentage (15% given as goal reduction), kilowatt hours (5965 kWh given), or USD ($656)" />

Additional data collected included:

-   **Energy Literacy Quiz**: An 8-item questionnaire assessing participants' knowledge of energy consumption and conversion (DeWaters & Powers, 2011).
-   **Calculator Usage Tracking**: Questions determined whether participants used a calculator, paper/pen, or other methods to complete the tasks.

## Results

### Data Analysis

All preprocessing and analyses were carried out in R (Team, 2020) and the tidyverse package (Wickham et al., 2019). Mixed Bayesian regressions were fit using the brms package (Bürkner, 2017), with participants and family scenario (states) set as random effects.

<div id="tbl-s1-agg">

| Reference Class | Avg. % Change | % meeting goal (exact) | % meeting goal (close match) | Abs. Deviation | Log Abs. Deviation |
|:---------|:--------|:-------------|:-----------------|:---------|:-----------|
| kWh | 0.22 | 0.38 | 0.54 | 0.03 | -3.7 |
| Percentage | 0.21 | 0.22 | 0.40 | 0.06 | -3.1 |
| USD | 0.23 | 0.10 | 0.22 | 0.10 | -2.4 |

Table 1: Study 1: Summary of planning accuracy by reference class. The table shows performance as both the % of trials where participants matched the goal, and the mean absolute error from the target reduction goal
</div>

<a href="#tbl-s1-agg" class="quarto-xref">Table 1</a> that participants in the kWh condition met the target goal 38% of the time, compared to 22% for the Percentage condition and 10% for the USD condition. Moreover, the kWh reference class exhibited smaller deviations from the target reduction, suggesting that participants performed more accurately when the goal was framed in kWh rather than when percentages or USD.

As shown in <a href="#tbl-s1-agg" class="quarto-xref">Table 1</a>, participants in the kWh condition exactly met the target reduction goal 38% of the time, significantly outperforming those in the Percentage (22%) and USD (10%) conditions. Furthermore, the kWh reference class exhibited notably smaller mean absolute deviations (0.03) compared to Percentage (0.06) and USD (0.10), suggesting that presenting the reduction goal in absolute units facilitated more precise allocations.

We next categorized responses into three accuracy levels (exact match \[0% error\], minor deviations \[0.01--5%\], and large deviations \[\>5%\]) for our primary statistical modeling. Using Bayesian ordinal regression, we modeled the ordered accuracy outcome as a function of the reference class condition, while controlling for random variation across participants and family scenarios:

$$
\text{Accuracy Level} \sim \text{Reference Class} + \text{Calculator} + (1|\text{id}) + (1|\text{Family Scenario})
$$

This approach allowed us to estimate thresholds (intercepts) and regression coefficients that capture how different reference classes affect the likelihood of achieving higher accuracy categories. For each comparison, we provide posterior odds ratios (OR) and their 95% CIs. This approach allows the estimation of threshold parameters and regression coefficients that characterize how changes in predictor variables (such as the reference class: kWh, percentage, or USD) relate to probabilities of being in each accuracy category.

<div id="tbl-s1-reg">

| Parameter          | Estimate | CI_Lower | CI_Upper | pd   |
|:-------------------|:---------|:---------|:---------|:-----|
| Intercept\[1\]     | -4.21    | -5.90    | -2.58    | 1.00 |
| Intercept\[2\]     | -0.89    | -2.49    | 0.71     | 0.87 |
| refClassPercentage | 1.44     | 0.07     | 2.88     | 0.98 |
| refClassUSD        | 3.13     | 1.81     | 4.50     | 1.00 |
| calcUsedCalculator | -3.30    | -4.80    | -1.92    | 1.00 |

Table 2: **Experiment 1**: Ordinal Regression results. Ordinal regression results. Positive coefficients for the reference class predictors indicate that those conditions are associated with higher error categories relative to the kWh baseline.
</div>
<div id="tbl-s1-ord">

| Comparison        | odds_ratio | ci_lower | ci_upper |
|:------------------|:-----------|:---------|:---------|
| Percentage vs kWh | 4.2        | 1.1      | 18       |
| USD vs kWh        | 22.9       | 6.1      | 90       |

Table 3: **Experiment 1**: Odds ratios for group comparisons. Odds ratios greater than 1 indicate increased odds of falling into a worse accuracy category compared to the kWh condition.
</div>

As shown in <a href="#tbl-s1-reg" class="quarto-xref">Table 2</a>, the reference class coefficients are positive for both the Percentage (Estimate = 1.3, 95% CI: 0.01 to 2.66, pd = 0.98) and USD (Estimate = 2.8, 95% CI: 1.52 to 4.04, pd = 1.00) conditions, relative to the kWh baseline. This indicates that, compared to the kWh condition, participants in both the Percentage and USD conditions were more likely to produce plans that fell into higher error categories. Moreover, the odds ratios (see Table 3) suggest that the USD condition led to a notably higher likelihood of large errors compared to the kWh baseline (OR = 15.7), while the Percentage condition also demonstrated increased odds (OR = 3.7) but was somewhat less detrimental to accuracy than USD. These results align with our descriptive findings and further clarify that framing the target reductions in absolute kWh units may facilitate significantly more accurate planning. Posterior predictive checks showed that the ordinal model provided a reasonable fit to the observed data (see **?@fig-s1-ppd**).

To further investigate individual factors that may influence planning accuracy, we examined the relationship between participants' energy literacy scores and their performance on the task. Energy literacy was assessed using an 8-item questionnaire adapted from (DeWaters & Powers, 2011), which covers topics such as energy units, appliance energy consumption, and sources of electricity. A Bayesian linear regression model was fit with log-transformed absolute error as the outcome variable and energy literacy score as the predictor, controlling for random effects of participant and state: log_abs_error ~ els + (1\|id) + (1\|state). Results indicated a significant negative relationship between energy literacy and log absolute error (Estimate = -2.35, 95% CI: -2.88 to -1.81), suggesting that participants with higher energy literacy scores tended to have smaller deviations from the target reduction goal, and thus more accurate plans overall (**?@fig-s1-els**).

## Experiment 1: Discussion

Experiment 1 examined how different numerical representations of energy reduction goals affected participants' planning accuracy. In line with our hypothesis that absolute units would yield better accuracy, the kWh condition supported significantly more precise energy reduction plans than did either the Percentage or USD conditions. Although the Percentage format was detrimental to accuracy relative to kWh, it was the USD condition that consistently produced the poorest outcomes, suggesting that monetary terms, while intuitive in everyday contexts, may not serve as effective reference classes for planning appliance-specific reductions in energy use.

Experiment 2 will extend these findings by examining whether additional variables, such as the difficulty of the reduction goal or the rounding of numerical values, further interact with reference class conditions, thereby providing a more comprehensive understanding of how to optimize energy information presentation for improved planning accuracy.

# Experiment 2

## Methods

The experimental procedures in Experiment 2 are quite similar to those in Experiment 1. Experiment 2 employed a 2 (task goal: 10% vs. 15% reduction) x 2 (last year's usage: exact vs. rounded) within-subjects design, with a between-subjects manipulation of the reference class (USD vs. Percentage vs. kWh).. We recruited 206 participants from Amazon Mechanical Turk, but data from from 10 participants were corrupted due to experimenter error, leaving a final sample of 196 participants.

## Results

<div id="tbl-s2-agg">

| Reference Class | % meeting goal (exact) | % meeting goal (close match) | Abs. Deviation | Log Abs. Deviation |
|:-----------|:---------------|:-------------------|:----------|:-------------|
| kWh | 0.44 | 0.52 | 0.02 | -3.9 |
| Percentage | 0.28 | 0.42 | 0.06 | -3.2 |
| USD | 0.20 | 0.29 | 0.10 | -2.4 |

Table 4: Experiment 2: Summary of planning accuracy by reference class. The table shows performance as both the % of trials where participants matched the goal, and the mean absolute error from the target reduction goal
</div>
<div id="tbl-s2-reg">

| Parameter          | Estimate | CI_Lower | CI_Upper | pd   |
|:-------------------|:---------|:---------|:---------|:-----|
| Intercept\[1\]     | -1.45    | -2.85    | -0.07    | 0.98 |
| Intercept\[2\]     | 1.26     | -0.09    | 2.65     | 0.97 |
| refClassPercentage | 1.02     | -0.63    | 2.71     | 0.89 |
| refClassUSD        | 2.27     | 0.53     | 3.98     | 0.99 |
| calcNoCalculator   | 4.10     | 2.20     | 6.06     | 1.00 |
| pct_goal15%        | -0.39    | -0.81    | 0.04     | 0.96 |
| roundedRounded     | -0.53    | -0.96    | -0.11    | 0.99 |

Table 5: **Experiment 2.** Parameter estimates from the ordinal regression model. Positive coefficients for refClass predictors indicate increased likelihood of falling into higher error categories relative to the kWh baseline.
</div>
<div id="tbl-s2-ord">

| comparison           | odds_ratio | ci_lower | ci_upper |
|:---------------------|:-----------|:---------|:---------|
| Percentage vs kWh    | 2.78       | 0.53     | 15.0     |
| USD vs kWh           | 9.68       | 1.69     | 53.4     |
| calcNoCalculator     | 60.37      | 9.02     | 426.4    |
| 15% Goal vs 10% Goal | 0.68       | 0.44     | 1.0      |
| Rounded vs Not       | 0.59       | 0.38     | 0.9      |

Table 6: **Experiment 2.** Odds ratios for group comparisons. Odds ratios greater than 1 indicate increased odds of falling into a worse accuracy category compared to the comparison condition.
</div>

As in Experiment 1, accuracy was categorized into three ordinal levels: "Exact match" (0% error), "0.01-5% error," and "Over 5% error". The analyses for Experiment 2 employed a Bayesian ordinal regression model to examine the probability of falling into one of three accuracy categories (exact match, minor deviations, or substantial deviations) as a function of the reference class condition (kWh, Percentage, USD), while including pct_goal (10% vs. 15%), rounded (exact vs. rounded usage data), and calculator usage as additional predictors. Random intercepts were specified for both participant and state,

The ordinal regression analysis revealed that the USD reference class significantly increased the odds of higher error categories compared to the kWh reference class (OR = 9.68, 95% CI: \[1.69, 53.4\]). Participants in the USD condition were therefore substantially more likely to deviate from the target energy reduction goal compared to those in the kWh condition. In contrast, the Percentage condition's odds ratio relative to kWh was more uncertain (OR = 2.78, 95% CI: 0.53, 15.0), indicating that although there may be a trend toward reduced accuracy in the Percentage condition, the evidence was not definitive.

We also found that using rounded numbers modestly improved accuracy (b = -0.53, 95% CI: \[-0.96, -0.11\]), with participants having 0.59 times the odds of falling into a worse accuracy category when working with rounded values. The more challenging 15% reduction goal was associated with slightly better performance compared to the 10% goal (b = -0.39, 95% CI: \[-0.81, 0.04\]), though this effect was relatively small. Consistent with Experiment 1, the use of a calculator had a large and significant effect on accuracy. The coefficient for calcNoCalculator was 4.10 (95% CI: 2.20, 6.06), and the corresponding odds ratio was 60.37 (95% CI: 9.02, 426.4), indicating that participants who did not use a calculator were substantially more likely to fall into higher error categories.

**?@fig-s2-ame** shows the marginal effects of refClass on each level of accuracy_level. These results reveal that switching from kWh to Percentage decreased the probability of an "Exact match" by an average of 7.0 percentage points (95% CI: -19.2, 4.2) and increased the probability of "Over 5% error" by 6.9 percentage points (95% CI: -4.5, 18.6). Similarly, switching from kWh to USD decreased the probability of an "Exact match" by 15 percentage points (95% CI: -26.7, -3.3) and increased the probability of "Over 5% error" by 16.5 percentage points (95% CI: 3.7, 29.3).

We once again examained the effect of energy literacy on planning accuracy. A Bayesian linear regression model was fit with log-transformed absolute error as the outcome variable and energy literacy score as the predictor, controlling for random effects of participant and state: log_abs_error ~ els + (1\|id) + (1\|state). This revealed a significant negative relationship between energy literacy and log absolute error (Estimate = -3.21, 95% CI: -3.89 to -2.52), indicating that participants with higher energy literacy scores tended to have smaller deviations from the target reduction goal, and thus more accurate plans overall (**?@fig-s2-els**).

## Experiment 2: Discussion

Experiment 2 built upon the findings of Experiment 1 by incorporating additional manipulations of goal difficulty (10% vs. 15% reduction) and numerical presentation (rounded vs. exact numbers), while maintaining the core manipulation of reference class (kWh, Percentage, USD). Nevertheless, the results largely converged with those of Experiment 1, providing further converging evidence that presenting energy reduction goals in absolute units (kWh) facilitates more accurate planning compared to percentage-based or monetary formats.

Taken together, the results of Experiment 2 provide further support for the hypothesis that presenting energy reduction goals in absolute units (kWh) leads to more accurate planning compared to percentage-based or monetary formats.

The effect of goal difficulty, with the more challenging 15% reduction goal associated with slightly better performance, although counterintuitive, was nonetheless relatively small and may warrant further investigation in future research. The large and significant effect of calculator use, consistent across both experiments, underscores the crucial role of tools that individuals are likely to employ in real-world settings. Finally, the consistent relationship between energy literacy and accuracy, observed across both experiments, highlights the potential value of educational interventions aimed at improving consumers' understanding of energy concepts.

# General Discusion

Across two experiments, we consistently found that presenting energy reduction goals in absolute units (kWh) led to more accurate planning compared to percentage-based or monetary representations. This advantage persisted across variations in goal difficulty and numerical presentation, suggesting a robust effect of reference class on planning accuracy.

Karjalainen 2011 - people prefer information about price (Karjalainen, 2011)

Mention Blasch et al. (2019) - better long-term appliance selection with information presented in monetary terms.



# References
