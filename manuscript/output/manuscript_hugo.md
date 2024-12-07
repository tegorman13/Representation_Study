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
    code-summary: Show the code
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
  gfm:
    echo: true
    output-file: manuscript_gfm.md
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
  hikmah-manuscript-docx: default
date: 2024-12-07T00:00:00.000Z
abstract: >
  This study aims to examine how the format of energy information impacts
  individuals’ ability to develop precise energy reduction plans. By
  manipulating the reference class (kWh, %, USD) and assessing planning
  accuracy, we seek to determine which format facilitates better comprehension
  and decision-making.Across two experiments, the kWh format generally led to
  better accuracy, while the USD format consistently led to the worst
  performance. These findings highlight the importance of effective information
  presentation to promote energy conservation, and may contribute to the
  development of more effective energy communication strategies that can enhance
  conservation efforts.
---


# Introduction

-   highlight the significant contribution of residential energy consumption to carbon emissions and the potential for substantial reductions. Make point about urgency of climate change? (IPCC (2014) and EIA (2012) ).

-   Electricity bills are a primary source of energy-use information for consumers and offer a promising avenue for enhancing communication about energy consumption (Fischer, 2008)

### Literature Review

-   Canfield et al. (2017) found that tables were more effective than graphs for conveying specific electricity usage data, likely because tables facilitate straightforward point reading. However, they also noted that individuals with lower energy literacy had reduced comprehension across all formats.
-   Canfield et al. (2017)'s findings on preferences for historical use information and the impact of neighbor comparisons
-   The concept of cognitive fit posits that performance improves when the information presentation format aligns with the task requirements (Vessey, 1991)
-   alignment can reduce cognitive load and enhance accuracy in planning (Shah & Freedman, 2011)
-   Reimer et al. (2015) provide context on how numerical formats affect risk perception, the reference class problem, and the benefits of natural frequencies.

Energy poverty continues to be a pervasive issue in the United States Memmott et al. (2021). This challenge partly arises from difficulties in converting information across numerical formats, impeding the development of precise energy reduction plans Reimer et al. (2015). Prior research by Canfield et al. (2017) demonstrated that presenting energy information in tabular formats enhances comprehension relative to graphs.

The way numerical information is presented can significantly affect how individuals process and use that information (Reimer et al., 2015) . The reference class problem highlights that numbers without clear reference points can lead to misinterpretation, as the meaning of a statistic depends on the category or class it refers to (Gigerenzer & Edwards, 2003; Reimer et al., 2015). Presenting energy information in absolute units (e.g., kWh) provides a clear reference class, potentially enhancing comprehension.

Furthermore, research suggests that natural frequencies and absolute numbers are generally easier for individuals to understand compared to percentages or probabilities Hoffrage et al. (2000). In the context of energy conservation, using absolute units may facilitate more accurate planning and decision-making by aligning with intuitive cognitive processing.

Despite existing studies on energy-use communication and format effects, limited research has explored how different numerical representations influence consumers' ability to create accurate energy conservation plans. Specifically, there is a gap in understanding how presenting energy information in absolute units versus percentages or monetary terms affects the precision of planning appliance-specific reductions. Addressing this gap is crucial for developing effective interventions that promote energy conservation behaviors.

### Hypotheses

Building on these findings and informed by prior work showing that frequencies (like absolute units in kWh) are easier to comprehend and facilitate more precise decision-making compared to percentages, our study also utilizes a tabular format, but manipulates whether participants must consider energy information presented as absolute units (kWh), percentages (%), or monetary costs (USD). We hypothesize that presenting information in absolute units (kWh) will lead to more accurate household energy conservation planning.

# Experiment 1

See <a href="#fig-task" class="quarto-xref">Figure 1</a> for an example of a planning trial as it was seen by participants.

## Methods

### Participants

We implemented our task and surveys on Qualtrics, and recruited participants through Amazon Mechanical Turk. In Experiment 1, 252 participants were intially recruited, but data from 17 participants were corrupted due to experimenter error, leaving a final sample of 235 participants. Most participants (76%) reported using a calculator to complete the task.

### Materials and Design

The study employed a mixed design with reference class (kWh, percentage, USD) as a between-subjects factor and state/family scenario as a within-subjects factor. Each participant completed energy reduction planning tasks for two different states, with state order counterbalanced across participants. The family scenarios featured four households in different climate regions: Texas (Smith family) and California (Adams family) representing warm climates, and Colorado (Wells family) and Massachusetts (Davis family) representing cold climates. We obtain average utility use from each state by CITE SOURCE FOR STATE AVGS?

### Procedure

Participants received energy usage data for two hypothetical families and were tasked with creating action plans to meet specified reduction goals by allocating usage across five appliance categories: heating, cooling, water heating, refrigerator, and an other appliances (e.g., TV, lighting).

For each family scenario, the particpipants were shown a table containing the families utility usage from the prior year, alongside the state averages for each appliance category (both prior year usage and stage averages are always shown in kWh). For each scenario, participants were asked to create two possible action plans to achieve the target reduction in total household energy usage (see <a href="#fig-task" class="quarto-xref">Figure 1</a>). Depending on their reference class condition, the target reduction amount presented either in kilowatt-hours (kWh), as percentages of total household usage, or in U.S. dollars. In all conditions, the target reduction was equivalent to a 15% reduction in total household kWh.

<img src="./assets/images/Smith_10R_Wells_15E.png" id="fig-task"
alt="Figure 1: Example trial in the energy planning task. Participants are shown the prior year electricity use of a household, and are tasked with creating a plan for the next year that will meet the energy reduction goal. Study 1 manipulates the format of the reduction goal to be either a percentage (15% given as goal reduction), kilowatt hours (5965 kWh given), or USD ($656)" />

Additional data collected included:

-   **Energy Literacy Quiz**: An 8-item questionnaire assessing participants' knowledge of energy consumption and conversion.
-   **Calculator Usage Tracking**: Questions determined whether participants used a calculator, paper/pen, or other methods to complete the tasks.
-   **Demographic Survey**: Collected information on gender, age, income, education, employment status, and state of residence.
-   **Environmental Attitudes Survey**: Assessed participants' pro-environmental attitudes and perceived importance of energy conservation.

## Results

For our primary analyses of participants' ability to create accurate energy-saving plans, we employed an accuracy level binning approach by categorizing responses into four distinct levels: Exact match, 0.01--2% error, 2.01--15% error, and Over 15% error. The current analysis employs a cumulative ordinal regression model, implemented via a Bayesian hierarchical framework (Bürkner, 2017) . This approach allows the estimation of threshold parameters and regression coefficients that characterize how changes in predictor variables (such as the reference class: kWh, percentage, or USD) relate to probabilities of being in each accuracy category. All analyses were carried out in R (Team, 2020) and the tidyverse package (Wickham et al., 2019).

see **?@fig-s1-plot**

We analyzed planning accuracy using Bayesian ordinal regression. The dependent variable, plan error, was computed by binning the goal deviation into four ordered levels: exact match (0% error), minor deviations (0.01-2% error), moderate deviations (2.01-15% error), and major deviations (\>15% error). For each comparison, we provide posterior odds ratios (OR) and their 95% CIs.

<div id="tbl-s1-or">

| Parameter          | Estimate | CI_Lower | CI_Upper | pd   |
|:-------------------|:---------|:---------|:---------|:-----|
| Intercept\[1\]     | -1.94    | -3.19    | -0.74    | 1.00 |
| Intercept\[2\]     | 0.20     | -1.02    | 1.42     | 0.63 |
| Intercept\[3\]     | 4.36     | 3.11     | 5.65     | 1.00 |
| refClassPercentage | 0.85     | -0.54    | 2.23     | 0.88 |
| refClassUSD        | 2.72     | 1.41     | 4.06     | 1.00 |

Table 1: **Experiment 1**: Ordinal Regression results.
</div>
<div id="tbl-s1-epred">

| Term               | Estimate | Est.Error | Q2.5 | Q97.5 |
|:-------------------|:---------|:----------|:-----|:------|
| refClassPercentage | 2.3      | 2         | 0.58 | 9.3   |
| refClassUSD        | 15.2     | 2         | 4.11 | 58.1  |

Table 2: **Experiment 1**: Odds ratios for group comparisons.
</div>

The ordinal model is parameterized with thresholds (intercepts), and positive coefficients can indicate that it is more difficult to achieve higher accuracy categories in the USD condition.
The model output suggests that, compared to the kWh condition, the USD condition shows a positive coefficient (Estimate = 2.72, 95% CI: 1.41 to 4.06) for the ordinal outcome. At least to me, this positive coefficient appears to indicate that, relative to the kWh reference class, participants in the USD condition are more likely to fall into higher numerical categories of the dependent variable coding. However, because the dependent variable is ordered from best (Exact match) to worst (Over 15% error), care is needed in interpretation. The Percentage condition coefficient (Estimate = 0.85, 95% CI: -0.54 to 2.23) is more uncertain, with its credible interval overlapping zero. Posterior predictive checks (Figure 3) showed that the ordinal model provided a reasonable fit to the observed data (see **?@fig-s1-plot2**).

# Experiment 2

## Methods

The experimental procedures in study 2 are quite similar to those in study 1, but we also included a rounding manipulation (rounded vs. not rounded), and a manipulation of the goal (10% reduction vs. 15% rediction). We recruited 206 participants from Amazon Mechanical Turk, but data from from 10 participants were corrupted due to experimenter error, leaving a final sample of 196 participants.

Note that reference class remains a between-subjects variable, while percent goal, rounding, and state are within-subjects variables. In study 2, the new design is a 4 state temperature (2 warm vs. 2 cold states) X 2 task goal (10% vs. 15%) X 2 last year's usage for the family and the state average (exact vs. rounded numbers) within X 3 task reference class (USD vs. Percentage vs. kWh) between.

## Results

<div id="tbl-s2-or">

| Parameter          | Estimate | CI_Lower | CI_Upper | pd   |
|:-------------------|:---------|:---------|:---------|:-----|
| Intercept\[1\]     | -2.13    | -3.39    | -0.86    | 1.00 |
| Intercept\[2\]     | -0.62    | -1.89    | 0.63     | 0.84 |
| Intercept\[3\]     | 3.15     | 1.88     | 4.42     | 1.00 |
| refClassPercentage | 0.83     | -0.64    | 2.33     | 0.87 |
| refClassUSD        | 1.87     | 0.31     | 3.35     | 0.99 |
| roundedRounded     | -0.66    | -1.01    | -0.31    | 1.00 |
| pct_goal15%        | -0.44    | -0.79    | -0.10    | 0.99 |

Table 3: **Experiment 2.** Ordinal Regression Model Results.
</div>
<div id="tbl-s2-epred">

| comparison           | odds_ratio | ci_lower | ci_upper |
|:---------------------|-----------:|---------:|---------:|
| Percentage vs kWh    |       3.02 |     0.53 |    10.31 |
| USD vs kWh           |       8.80 |     1.37 |    28.42 |
| Rounded vs Not       |       0.53 |     0.36 |     0.73 |
| 15% Goal vs 10% Goal |       0.66 |     0.45 |     0.91 |

Table 4: **Experiment 2.** Odds ratios for group comparisons.
</div>

### Individual Differences

see **?@fig-s2-indv**

# Discusion

Karjalainen 2011 - people prefer information about price (Karjalainen, 2011)

# References

Bürkner, P.-C. (2017). Brms: An R Package for Bayesian Multilevel Models Using Stan. *Journal of Statistical Software*, *80*, 1--28. <https://doi.org/10.18637/jss.v080.i01>

Canfield, C., Bruine De Bruin, W., & Wong-Parodi, G. (2017). Perceptions of electricity-use communications: Effects of information, format, and individual differences. *Journal of Risk Research*, *20*(9), 1132--1153. <https://doi.org/10.1080/13669877.2015.1121909>

Fischer, C. (2008). Feedback on household electricity consumption: A tool for saving energy? *Energy Efficiency*, *1*(1), 79--104. <https://doi.org/10.1007/s12053-008-9009-7>

Gigerenzer, G., & Edwards, A. (2003). Simple tools for understanding risks: From innumeracy to insight. *BMJ*, *327*(7417), 741--744. <https://doi.org/10.1136/bmj.327.7417.741>

Gigerenzer, G., & Hoffrage, U. (1995). How to improve Bayesian reasoning without instruction: Frequency formats. *Psychological Review*, *102*(4), 684--704. <https://doi.org/10.1037/0033-295X.102.4.684>

Hoffrage, U., Lindsey, S., Hertwig, R., & Gigerenzer, G. (2000). Communicating Statistical Information. *Science*, *290*(5500), 2261--2262. <https://doi.org/10.1126/science.290.5500.2261>

Karjalainen, S. (2011). Consumer preferences for feedback on household electricity consumption. *Energy and Buildings*, *43*(2-3), 458--467. <https://doi.org/10.1016/j.enbuild.2010.10.010>

Memmott, T., Carley, S., Graff, M., & Konisky, D. M. (2021). Sociodemographic disparities in energy insecurity among low-income households before and during the COVID-19 pandemic. *Nature Energy*, *6*(2), 186--193. <https://doi.org/10.1038/s41560-020-00763-9>

Reimer, T., Jones, C., & Skubisz, C. (2015). Numeric Communication of Risk. In *The SAGE handbook of risk communication* (pp. 167--179).

Shah, P., & Freedman, E. G. (2011). Bar and Line Graph Comprehension: An Interaction of Top-Down and Bottom-Up Processes. *Topics in Cognitive Science*, *3*(3), 560--578. <https://doi.org/10.1111/j.1756-8765.2009.01066.x>

Team, R. C. (2020). *R: A Language and Environment for Statistical Computing*. R: A Language and Environment for Statistical Computing.

Vessey, I. (1991). Cognitive Fit: A Theory-Based Analysis of the Graphs Versus Tables Literature. *Decision Sciences*, *22*(2), 219--240. <https://doi.org/10.1111/j.1540-5915.1991.tb00344.x>

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., ... Yutani, H. (2019). Welcome to the Tidyverse. *Journal of Open Source Software*, *4*(43), 1686. <https://doi.org/10.21105/joss.01686>
