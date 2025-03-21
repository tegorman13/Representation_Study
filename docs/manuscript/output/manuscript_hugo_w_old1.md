
# Planning to Save Energy: How Information Format Affects Accuracy

## Abstract

Effective communication of energy consumption information is crucial for promoting residential energy conservation. This study investigates how different numerical representations of energy reduction goals influence consumers' ability to create accurate conservation plans. Across two experiments, we examined the impact of presenting energy information in kilowatt-hours (kWh), percentages, or U.S. dollars (USD) on planning accuracy. Participants completed a simulated household planning task in which they allocated energy usage across multiple appliances, with the goal presented in either kilowatt-hours (kWh), percentages, or monetary costs. Results across both experiments showed that presenting reduction goals in absolute units (kWh) led to significantly greater accuracy compared to percentage-based or monetary formats. Furthermore, we found that higher energy literacy was associated with more accurate planning. These findings demonstrate that absolute units (kWh) are more effective for communicating energy-saving goals, and highlight the potential value of educational interventions to improve consumer energy literacy.

# Introduction

### Literature Review

Energy costs often impose a significant burden on low-income households, leading to "energy insecurity," where basic energy needs cannot be met (Bednar & Reames, 2020). Frequently measured as the energy burden—the percentage of income spent on energy bills—this burden can be disproportionately high for vulnerable families, necessitating difficult trade-offs with essentials such as food or medicine (Bednar & Reames, 2020; Memmott et al., 2021). Energy insecurity has been linked to health risks and unsafe coping strategies, disproportionately impacting racial and ethnic minorities through higher rates of disconnection (Memmott et al., 2021). While the broader context of climate change, partly driven by residential consumption (Farghali et al., 2023), underscores the need for sustainable solutions, the financial strain on vulnerable households remains a pressing concern. Promoting behavior change to reduce energy consumption is crucial. However, the success of such interventions may hinge on how effectively energy information is communicated, with format and clarity significantly influencing understanding and action.


The way numerical information is presented can significantly affect how individuals process and use that information. Of particular relevance are reference class effects, which occur when numerical statements are presented without a clear or intuitive basis for comparison, thereby hindering the meaningful inference of quantities (Gigerenzer & Edwards, 2003; Reimer et al., 2015). A substantial body of evidence suggests that presenting data in terms of absolute counts or frequencies, as opposed to probabilities or percentages, can promote more accurate comprehension and facilitate more effective reasoning, particularly in domains requiring precise judgments (Gigerenzer & Hoffrage, 1995; Hoffrage, Lindsey, Hertwig, & Gigerenzer, 2000). However, it's important to note that even intuitive formats can pose challenges. Weber et al. (2018) found that individuals often struggle with reasoning tasks presented in natural frequencies because they inadvertently revert to more complex probabilistic thinking. 

The units and format of information presentation has also been repeatedly shown to influence decision making in the context of energy consumption and planning. For instance, tables are generally more effective than graphs for conveying specific electricity usage data because they facilitate straightforward point reading (Canfield et al., 2017). Furthermore, the framing of energy costs, such as displaying monthly rather than daily or yearly expenses, can significantly affect consumers' choices (Gill et al., 2022). 

Consequently, the selection of an appropriate information format is crucial for effectively supporting energy-related decisions. To that end, similar to natural frequencies, kWh provide a direct measure of energy use, a characteristic that could simplify calculations and facilitate comparisons, potentially helping consumers better understand and compare the energy consumption of different appliances or activities.  On the other hand, some prior research has suggested that consumers have a preference for receiving energy feedback in terms of monetary values over scientific units (Karjalainen 2011; Nemati & Penn, 2020), as well as better long-term appliance selection with information presented in monetary terms (Blasch et al., 2019). It thus remains uncertain how these reported benefits might generalize to household energy planning, where usage patterns can be unusually multifaceted and subject to contextual influences. 

Although many individuals express a desire to conserve energy, research consistently shows that abstract goals (e.g., “reduce overall usage by 15%”) often fail to translate into effective behavior change unless accompanied by specific, actionable steps (Abrahamse et al., 2005; Nemati & Penn, 2020). For instance, Abrahamse et al. (2005) demonstrated that merely providing general information about energy savings rarely alters consumption patterns unless consumers also receive concrete instructions or tailored feedback. Similarly, Tonke (2024) reported that sending households brief but precise text messages outlining how to reduce water use (e.g., limiting irrigation times, adjusting washing machine settings) yielded meaningful decreases in consumption, underscoring the importance of procedural knowledge—namely, knowing how to operationalize a goal rather than simply why it is desirable. In the context of energy conservation, this implies that interventions should not only highlight potential reductions (such as a 15% target) but also guide residents in allocating those reductions across specific appliances or behaviors (Attari & Rajagopal, 2015). Additionally, meta-analytic findings suggest that people respond more robustly to household-level feedback that situates their usage within a personalized framework, thereby reducing the cognitive burden of figuring out next steps (Nemati & Penn, 2020). 

Despite existing studies on energy-use communication and format effects, limited research has explored how different numerical representations influence consumers’ ability to create accurate energy conservation plans. Specifically, there is a gap in understanding how presenting energy information in absolute units versus percentages or monetary terms affects the precision of planning appliance-specific reductions. The current study addresses these critical issues by systematically investigating the impact of varying information formats (kWh, percentage, and USD) on the accuracy of energy-planning decisions. By manipulating the presentation format of energy information, this research aims to elucidate how different representational formats influence planning accuracy.





### Hypotheses



# Experiment 1

## Methods

### Participants

We implemented our task and surveys on Qualtrics, and recruited participants through Amazon Mechanical Turk. In Experiment 1, 252 participants were initially recruited, but data from 17 participants were corrupted due to experimenter error, leaving a final sample of 235 participants. Most participants (76%) reported using a calculator to complete the task.

### Materials and Design

The study employed a mixed design with reference class (kWh, percentage, USD) as a between-subjects factor and state/family scenario as a within-subjects factor. Each participant completed energy reduction planning tasks for two different states, with state order counterbalanced across participants. The family scenarios featured four households in different climate regions: Texas (Smith family) and California (Adams family) representing warm climates, and Colorado (Wells family) and Massachusetts (Davis family) representing cold climates. We obtain average utility use from each state by CITE SOURCE FOR STATE AVGS?

### Procedure

Participants received energy usage data for two hypothetical families and were tasked with creating action plans to meet specified reduction goals by allocating usage across five appliance categories: heating, cooling, water heating, refrigerator, and and other appliances (e.g., TV, lighting).

For each family scenario, the participants were shown a table containing the families utility usage from the prior year, alongside the state averages for each appliance category (both prior year usage and state averages are always shown in kWh). For each scenario, participants were asked to create two possible action plans to achieve the target reduction in total household energy usage Figure 1. Depending on their reference class condition, the target reduction amount presented either in kilowatt-hours (kWh), as percentages of total household usage, or in U.S. dollars. In all conditions, the target reduction was equivalent to a 15% reduction in total household kWh.



Additional data collected included:

-   **Energy Literacy Quiz**: An 8-item questionnaire assessing participants' knowledge of energy consumption and conversion (DeWaters & Powers, 2011).
-   **Calculator Usage Tracking**: Questions determined whether participants used a calculator, paper/pen, or other methods to complete the tasks.

## Results

### Data Analysis

All preprocessing and analyses were carried out in R (Team, 2020) and the tidyverse package (Wickham et al., 2019). Mixed Bayesian regressions were fit using the brms package (Bürkner, 2017), with participants and family scenario (states) set as random effects.


| Reference Class | Avg. % Change | % meeting goal (exact) | % meeting goal (close match) | Abs. Deviation | Log Abs. Deviation |
|:---------|:--------|:-------------|:-----------------|:---------|:-----------|
| kWh | 0.22 | 0.38 | 0.54 | 0.03 | -3.7 |
| Percentage | 0.21 | 0.22 | 0.40 | 0.06 | -3.1 |
| USD | 0.23 | 0.10 | 0.22 | 0.10 | -2.4 |

Table 1: Study 1: Summary of planning accuracy by reference class. The table shows performance as both the % of trials where participants matched the goal, and the mean absolute error from the target reduction goal
</div>

able 1</a> that participants in the kWh condition met the target goal 38% of the time, compared to 22% for the Percentage condition and 10% for the USD condition. Moreover, the kWh reference class exhibited smaller deviations from the target reduction, suggesting that participants performed more accurately when the goal was framed in kWh rather than when percentages or USD.

As shown in Table 1 participants in the kWh condition exactly met the target reduction goal 38% of the time, significantly outperforming those in the Percentage (22%) and USD (10%) conditions. Furthermore, the kWh reference class exhibited notably smaller mean absolute deviations (0.03) compared to Percentage (0.06) and USD (0.10), suggesting that presenting the reduction goal in absolute units facilitated more precise allocations.

We next categorized responses into three accuracy levels (exact match \[0% error\], minor deviations \[0.01--5%\], and large deviations \[\>5%\]) for our primary statistical modeling. Using Bayesian ordinal regression, we modeled the ordered accuracy outcome as a function of the reference class condition, while controlling for random variation across participants and family scenarios:

$$
\text{Accuracy Level} \sim \text{Reference Class} + \text{Calculator} + (1|\text{id}) + (1|\text{Family Scenario})
$$

This approach allowed us to estimate thresholds (intercepts) and regression coefficients that capture how different reference classes affect the likelihood of achieving higher accuracy categories. For each comparison, we provide posterior odds ratios (OR) and their 95% CIs. This approach allows the estimation of threshold parameters and regression coefficients that characterize how changes in predictor variables (such as the reference class: kWh, percentage, or USD) relate to probabilities of being in each accuracy category. Specifically, we used a cumulative logit link function to model the ordered accuracy outcome, and we specified weakly informative priors for the regression coefficients (normal distributions with mean 0 and standard deviation of 1) and for the cutpoints (normal distributions with a mean of zero and a standard deviation of 4.0). The approach allows us to estimate threshold parameters and regression coefficients that characterize how changes in predictor variables (such as the reference class: kWh, percentage, or USD) relate to probabilities of being in each accuracy category.


| Parameter          | Estimate | CI_Lower | CI_Upper | pd   |
|:-------------------|:---------|:---------|:---------|:-----|
| Intercept\[1\]     | -4.21    | -5.90    | -2.58    | 1.00 |
| Intercept\[2\]     | -0.89    | -2.49    | 0.71     | 0.87 |
| refClassPercentage | 1.44     | 0.07     | 2.88     | 0.98 |
| refClassUSD        | 3.13     | 1.81     | 4.50     | 1.00 |
| calcUsedCalculator | -3.30    | -4.80    | -1.92    | 1.00 |

Table 2: **Experiment 1**: Ordinal Regression results. Ordinal regression results. Positive coefficients for the reference class predictors indicate that those conditions are associated with higher error categories relative to the kWh baseline.



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

The experimental procedures in Experiment 2 are quite similar to those in Experiment 1. Experiment 2 employed a 2 (task goal: 10% vs. 15% reduction) x 2 (last year's usage: exact vs. rounded) within-subjects design, with a between-subjects manipulation of the reference class (USD vs. Percentage vs. kWh). We recruited 206 participants from Amazon Mechanical Turk, but data from from 10 participants were corrupted due to experimenter error, leaving a final sample of 196 participants.

## Results


| Reference Class | % meeting goal (exact) | % meeting goal (close match) | Abs. Deviation | Log Abs. Deviation |
|:-----------|:---------------|:-------------------|:----------|:-------------|
| kWh | 0.44 | 0.52 | 0.02 | -3.9 |
| Percentage | 0.28 | 0.42 | 0.06 | -3.2 |
| USD | 0.20 | 0.29 | 0.10 | -2.4 |

Table 4: Experiment 2: Summary of planning accuracy by reference class. The table shows performance as both the % of trials where participants matched the goal, and the mean absolute error from the target reduction goal

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

We once again examined the effect of energy literacy on planning accuracy. A Bayesian linear regression model was fit with log-transformed absolute error as the outcome variable and energy literacy score as the predictor, controlling for random effects of participant and state: log_abs_error ~ els + (1\|id) + (1\|state). This revealed a significant negative relationship between energy literacy and log absolute error (Estimate = -3.21, 95% CI: -3.89 to -2.52), indicating that participants with higher energy literacy scores tended to have smaller deviations from the target reduction goal, and thus more accurate plans overall (**?@fig-s2-els**).

## Experiment 2: Discussion

Experiment 2 aimed to build upon the findings of Experiment 1, not only by replicating the core manipulation of reference class, but also by incorporating additional variables that might influence planning accuracy. These included goal difficulty and the way that the prior year's usage was presented (rounded or exact). The results largely converged with those of Experiment 1, providing further converging evidence that presenting energy reduction goals in absolute units (kWh) facilitates more accurate planning compared to percentage-based or monetary formats.

Taken together, the results of Experiment 2 provide further support for the hypothesis that presenting energy reduction goals in absolute units (kWh) leads to more accurate planning compared to percentage-based or monetary formats.

The finding that the more challenging 15% reduction goal was associated with a slight improvement in accuracy is counterintuitive. It may be that participants put more effort into the task under this condition, or perhaps this is an artifact of the way that the task was presented. However, this effect was relatively small and thus should be explored in future research to better understand its underlying mechanisms. Furthermore, the magnitude of the effect size of this manipulation should be examined to better understand the practical implications of goal difficulty for energy conservation.

The large and significant effect of calculator use, consistent across both experiments, underscores the crucial role of tools that individuals are likely to employ in real-world settings. Finally, the consistent relationship between energy literacy and accuracy, observed across both experiments, highlights the potential value of educational interventions aimed at improving consumers' understanding of energy concepts.

# General Discussion

Across two experiments, we consistently found that presenting energy reduction goals in absolute units (kWh) led to more accurate planning compared to percentage-based or monetary representations. This advantage persisted across variations in goal difficulty and numerical presentation, suggesting a robust effect of reference class on planning accuracy.

It is, however, important to note that while we find a significant benefit of presenting energy information in absolute units, some prior work suggests that consumers may prefer to receive information about costs, rather than absolute units (Karjalainen, 2011), or in some cases, that monetary information may lead to better decision-making (Blasch et al., 2019).

Karjalainen 2011 - people prefer information about price (Karjalainen, 2011)

Mention Blasch et al. (2019) - better long-term appliance selection with information presented in monetary terms.
