---
title: 'Planning to Save Energy: How Information Format Affects Accuracy'
author:
  - name: Thomas E. Gorman
    url: https://tegorman13.github.io/
    orcid: 0000-0001-5366-5442
    affiliations:
      - ref: cclab
      - ref: clara
    attributes:
      corresponding: true
  - name: Torsten Reimer
    url: https://web.ics.purdue.edu/~treimer/
    orcid: 0000-0002-7419-0076
    affiliations:
      - ref: cclab
      - ref: clara
  - name: Juan Pablo Loaiza Ramirez
    url: >-
      https://www.cla.purdue.edu/directory/profiles/loaiza-ramirez,juan-pablo.html
    orcid: 0000-0001-9663-0522
    affiliations:
      - ref: cclab
      - ref: clara
  - name: Hayden Barber
    url: https://www.sdstate.edu/directory/hayden-barber
    orcid: 0000-0002-3465-8615
    affiliations:
      - ref: sdsu
affiliations:
  - id: cclab
    name: Communication and Cognition Lab, Purdue University
    city: West Lafayette
    region: IN
    country: USA
    postal-code: 47907
    url: https://web.ics.purdue.edu/~treimer/
  - id: clara
    name: College of Liberal Arts Research Academy
    department: Purdue University
    city: West Lafayette
    region: IN
    country: USA
    postal-code: 47907
    url: https://cla.purdue.edu/about/college-initiatives/research-academy/
  - id: sdsu
    name: School of Communication & Journalism, South Dakota State University
    city: Brookings
    region: SD
    country: USA
    url: https://www.sdstate.edu/directory/hayden-barber
format:
  wordcount-html:
    wordcount-banner: true
    theme:
      - zephyr
      - ../assets/custom.scss
    toc: true
    shift-heading-level-by: 1
    code-fold: true
    code-summary: Show the code
    title-block-banner: '#000D51'
    include-in-header:
      - text: |
          <style type="text/css">
          h2.hide {
              display: none;
          }
          </style>
  hugo-md:
    include: true
    echo: false
    html-math-method: mathjax
    output-file: test_hugo.md
  gfm:
    echo: true
    output-file: test_gfm.md
  hikmah-manuscript-pdf:
    output-file: test.pdf
    mainfont: Linux Libertine O
    mainfontoptions:
      - Numbers=Proportional
      - Numbers=OldStyle
    mathfont: Libertinus Math
    knitr:
      opts_chunk:
        dev: cairo_pdf
  hikmah-manuscript-docx: default
date: 2024-12-03T00:00:00.000Z
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


This study aims to examine how the format of energy information impacts individuals’ ability to develop precise energy reduction plans. By manipulating the reference class (kWh, %, USD) and assessing planning accuracy, we seek to determine which format facilitates better comprehension and decision-making.Across two experiments, the kWh format generally led to better accuracy, while the USD format consistently led to the worst performance. These findings highlight the importance of effective information presentation to promote energy conservation, and may contribute to the development of more effective energy communication strategies that can enhance conservation efforts.



# Introduction

-   highlight the significant contribution of residential energy consumption to carbon emissions and the potential for substantial reductions. Make point about urgency of climate change? (IPCC (2014) and EIA (2012) ).

-   Electricity bills are a primary source of energy-use information for consumers and offer a promising avenue for enhancing communication about energy consumption (Fischer, 2008)

### Literature Review

-   Canfield et al. (2017) found that tables were more effective than graphs for conveying specific electricity usage data, likely because tables facilitate straightforward point reading. However, they also noted that individuals with lower energy literacy had reduced comprehension across all formats.

-   Canfield et al. (2017)'s findings on preferences for historical use information and the impact of neighbor comparisons

-   alignment can reduce cognitive load and enhance accuracy in planning (Shah & Freedman, 2011)

-   Reimer et al. (2015) provide context on how numerical formats affect risk perception, the reference class problem, and the benefits of natural frequencies.

Energy poverty continues to be a pervasive issue in the United States Memmott et al. (2021). This challenge partly arises from difficulties in converting information across numerical formats, impeding the development of precise energy reduction plans Reimer et al. (2015). Prior research by Canfield et al. (2017) demonstrated that presenting energy information in tabular formats enhances comprehension relative to graphs.

The way numerical information is presented can significantly affect how individuals process and use that information (Reimer et al., 2015) . The reference class problem highlights that numbers without clear reference points can lead to misinterpretation, as the meaning of a statistic depends on the category or class it refers to (Gigerenzer & Edwards, 2003; Reimer et al., 2015). Presenting energy information in absolute units (e.g., kWh) provides a clear reference class, potentially enhancing comprehension.

Furthermore, research suggests that natural frequencies and absolute numbers are generally easier for individuals to understand compared to percentages or probabilities Hoffrage et al. (2000). In the context of energy conservation, using absolute units may facilitate more accurate planning and decision-making by aligning with intuitive cognitive processing.

Despite existing studies on energy-use communication and format effects, limited research has explored how different numerical representations influence consumers' ability to create accurate energy conservation plans. Specifically, there is a gap in understanding how presenting energy information in absolute units versus percentages or monetary terms affects the precision of planning appliance-specific reductions. Addressing this gap is crucial for developing effective interventions that promote energy conservation behaviors.

Abrahamse et al. (2005)

Attari & Rajagopal (2015)

DeWaters & Powers (2011)

Gigerenzer & Edwards (2003)

Larrick & Soll (2008)

Blasch et al. (2019)

Fischer (2008)

Schwartz et al. (2015)

von Grabe (2020)

Gigerenzer & Hoffrage (1995)

Attari et al. (2010)

Herrmann et al. (2018)

Van Den Broek & Walker (2019)

Herberz et al. (2020)

Marghetis et al. (2019)

Lagomarsino et al. (2022)

Hahnel et al. (2020)

(**delgadoOpportunitiesGreaterEnergy2018?**)

Schley & DeKay (2015)

Lange & Dewitte (2021)

Tiede et al. (2022)

### Hypotheses

Building on these findings and informed by prior work showing that frequencies (like absolute units in kWh) are easier to comprehend and facilitate more precise decision-making compared to percentages, our study also utilizes a tabular format, but manipulates whether participants must consider energy information presented as absolute units (kWh), percentages (%), or monetary costs (USD). We hypothesize that presenting information in absolute units (kWh) will lead to more accurate household energy conservation planning.

# Experiment 1

See **?@fig-task** for an example of a planning trial as it was seen by participants.

## Methods

### Participants

We implemented our task and surveys on Qualtrics, and recruited participants through Amazon Mechanical Turk. In Experiment 1, 252 participants were intially recruited, but data from 17 participants were corrupted due to experimenter error, leaving a final sample of 235 participants. Most participants (76%) reported using a calculator to complete the task.

### Materials and Design

The study employed a mixed design with reference class (kWh, percentage, USD) as a between-subjects factor and state/family scenario as a within-subjects factor. Each participant completed energy reduction planning tasks for two different states, with state order counterbalanced across participants. The family scenarios featured four households in different climate regions: Texas (Smith family) and California (Adams family) representing warm climates, and Colorado (Wells family) and Massachusetts (Davis family) representing cold climates. We obtain average utility use from each state by CITE SOURCE FOR STATE AVGS?

### Procedure

Participants received energy usage data for two hypothetical families and were tasked with creating action plans to meet specified reduction goals by allocating usage across five appliance categories: heating, cooling, water heating, refrigerator, and an other appliances (e.g., TV, lighting).

Additional data collected included:

-   **Energy Literacy Quiz**: An 8-item questionnaire assessing participants' knowledge of energy consumption and conversion.
-   **Calculator Usage Tracking**: Questions determined whether participants used a calculator, paper/pen, or other methods to complete the tasks.
-   **Demographic Survey**: Collected information on gender, age, income, education, employment status, and state of residence.
-   **Environmental Attitudes Survey**: Assessed participants' pro-environmental attitudes and perceived importance of energy conservation.


Abrahamse, W., Steg, L., Vlek, C., & Rothengatter, T. (2005). A review of intervention studies aimed at household energy conservation. *Journal of Environmental Psychology*, *25*(3), 273--291. <https://doi.org/10.1016/j.jenvp.2005.08.002>

Attari, S. Z., DeKay, M. L., Davidson, C. I., & Bruine De Bruin, W. (2010). Public perceptions of energy consumption and savings. *Proceedings of the National Academy of Sciences*, *107*(37), 16054--16059. <https://doi.org/10.1073/pnas.1001509107>

Attari, S. Z., & Rajagopal, D. (2015). Enabling energy conservation through effective decision aids. *Journal of Sustainability Education*, *8*, 1--15.

Blasch, J., Filippini, M., & Kumar, N. (2019). Boundedly rational consumers, energy and investment literacy, and the display of information on household appliances. *Resource and Energy Economics*, *56*, 39--58. <https://doi.org/10.1016/j.reseneeco.2017.06.001>

Canfield, C., Bruine De Bruin, W., & Wong-Parodi, G. (2017). Perceptions of electricity-use communications: Effects of information, format, and individual differences. *Journal of Risk Research*, *20*(9), 1132--1153. <https://doi.org/10.1080/13669877.2015.1121909>

DeWaters, J. E., & Powers, S. E. (2011). Energy literacy of secondary students in New York State (USA): A measure of knowledge, affect, and behavior. *Energy Policy*, *39*(3), 1699--1710. <https://doi.org/10.1016/j.enpol.2010.12.049>

Fischer, C. (2008). Feedback on household electricity consumption: A tool for saving energy? *Energy Efficiency*, *1*(1), 79--104. <https://doi.org/10.1007/s12053-008-9009-7>

Gigerenzer, G., & Edwards, A. (2003). Simple tools for understanding risks: From innumeracy to insight. *BMJ*, *327*(7417), 741--744. <https://doi.org/10.1136/bmj.327.7417.741>

Gigerenzer, G., & Hoffrage, U. (1995). How to improve Bayesian reasoning without instruction: Frequency formats. *Psychological Review*, *102*(4), 684--704. <https://doi.org/10.1037/0033-295X.102.4.684>

Hahnel, U. J. J., Chatelain, G., Conte, B., Piana, V., & Brosch, T. (2020). Mental accounting mechanisms in energy decision-making and behaviour. *Nature Energy*, *5*(12), 952--958. <https://doi.org/10.1038/s41560-020-00704-6>

Herberz, M., Brosch, T., & Hahnel, U. J. J. (2020). Kilo what? Default units increase value sensitivity in joint evaluations of energy efficiency. *Judgment and Decision Making*, *15*(6), 972--988. <https://doi.org/10.1017/S1930297500008172>

Herrmann, M. R., Brumby, D. P., Oreszczyn, T., & Gilbert, X. M. P. (2018). Does data visualization affect users' understanding of electricity consumption? *Building Research & Information*, *46*(3), 238--250. <https://doi.org/10.1080/09613218.2017.1356164>

Hoffrage, U., Lindsey, S., Hertwig, R., & Gigerenzer, G. (2000). Communicating Statistical Information. *Science*, *290*(5500), 2261--2262. <https://doi.org/10.1126/science.290.5500.2261>

Lagomarsino, M., van der Kam, M., Parra, D., & Hahnel, U. J. J. (2022). Do I need to charge right now? Tailored choice architecture design can increase preferences for electric vehicle smart charging. *Energy Policy*, *162*, 112818. <https://doi.org/10.1016/j.enpol.2022.112818>

Lange, F., & Dewitte, S. (2021). The Work for Environmental Protection Task: A consequential web-based procedure for studying pro-environmental behavior. *Behavior Research Methods*, *54*(1), 133--145. <https://doi.org/10.3758/s13428-021-01617-2>

Larrick, R. P., & Soll, J. B. (2008). The MPG Illusion. *Science*, *320*(5883), 1593--1594. <https://doi.org/10.1126/science.1154983>

Marghetis, T., Attari, S. Z., & Landy, D. (2019). Simple interventions can correct misperceptions of home energy use. *Nature Energy*, *4*(10), 874--881. <https://doi.org/10.1038/s41560-019-0467-2>

Memmott, T., Carley, S., Graff, M., & Konisky, D. M. (2021). Sociodemographic disparities in energy insecurity among low-income households before and during the COVID-19 pandemic. *Nature Energy*, *6*(2), 186--193. <https://doi.org/10.1038/s41560-020-00763-9>

Reimer, T., Jones, C., & Skubisz, C. (2015). Numeric Communication of Risk. In *The SAGE handbook of risk communication* (pp. 167--179).

Schley, D. R., & DeKay, M. L. (2015). Cognitive accessibility in judgments of household energy consumption. *Journal of Environmental Psychology*, *43*, 30--41. <https://doi.org/10.1016/j.jenvp.2015.05.004>

Schwartz, T., Stevens, G., Jakobi, T., Denef, S., Ramirez, L., Wulf, V., & Randall, D. (2015). What People Do with Consumption Feedback: A Long-Term Living Lab Study of a Home Energy Management System. *Interacting with Computers*, *27*(6), 551--576. <https://doi.org/10.1093/iwc/iwu009>

Shah, P., & Freedman, E. G. (2011). Bar and Line Graph Comprehension: An Interaction of Top-Down and Bottom-Up Processes. *Topics in Cognitive Science*, *3*(3), 560--578. <https://doi.org/10.1111/j.1756-8765.2009.01066.x>

Tiede, K. E., Bjälkebring, P., & Peters, E. (2022). Numeracy, numeric attention, and number use in judgment and choice. *Journal of Behavioral Decision Making*, *35*(3), e2264. <https://doi.org/10.1002/bdm.2264>

Van Den Broek, K. L., & Walker, I. (2019). Heuristics in energy judgement tasks. *Journal of Environmental Psychology*, *62*, 95--104. <https://doi.org/10.1016/j.jenvp.2019.02.008>

von Grabe, J. (2020). Using the Instance-Based Learning Paradigm to Model Energy-Relevant Occupant Behaviors in Buildings. *Cognitive Computation*, *12*(1), 71--99. <https://doi.org/10.1007/s12559-019-09672-w>
