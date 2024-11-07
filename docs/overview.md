---
title: Effect of Reference Class on Planning to Conserve
author:
  - name: Thomas E. Gorman
    url: https://tegorman13.github.io/
    affiliations:
      - name: Communication and Cognition Lab, Purdue University, USA
        affiliation-url: https://web.ics.purdue.edu/~treimer/
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
    output-file: overview.md
  gfm:
    echo: true
    output-file: overview.md
---


# Effect of Reference Class on Planning to Conserve
Thomas E. Gorman

<details class="code-fold">
<summary>Code</summary>

``` r
pacman::p_load(dplyr,purrr,tidyr,here,tibble,brms,rstan,bayestestR,emmeans,tidybayes,modelsummary,
               ggplot2,gt,knitr,kableExtra,ggh4x,lme4,flextable,pander)
options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)
walk(c("fun_plot"), ~ source(here::here(paste0("Scripts/", .x, ".R"))))
theme_set(theme_nice())

s1 <- readRDS(here::here("data/s1_processed.rds")) |> filter(!(id %in% readRDS(here::here("data/s1_discrep_ids.rds"))))
s2_long <- readRDS(here::here("data/s2_processed.rds")) |> filter(!(id %in% readRDS(here::here("data/s2_discrep_ids.rds"))))

state_code_map <- c(
  "CAL" = "California",
  "TEX" = "Texas",
  "COL" = "Colorado",
  "MASS" = "Massachusetts"
)


state_to_code <- state_code_map
code_to_state <- setNames(names(state_code_map), state_code_map)
lookup_code <- function(state) {
  if (state %in% names(state_to_code)) {
    return(state_to_code[[state]])
  } else if (state %in% names(code_to_state)) {
    return(code_to_state[[state]])
  } else {
    return(NA)  # Return NA if not found in either mapping
  }
}
```

</details>

<details class="code-fold">
<summary>Code</summary>

``` r
s1 |> group_by(refClass) |> 
  rename("Reference Class" = refClass) |>
  summarise(n=n_distinct(id)) |> 
  gt() |> tab_header(title="Study 1: Number of unique IDs per condition") 
```

</details>

<div id="txjwrdriwo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#txjwrdriwo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#txjwrdriwo thead, #txjwrdriwo tbody, #txjwrdriwo tfoot, #txjwrdriwo tr, #txjwrdriwo td, #txjwrdriwo th {
  border-style: none;
}
&#10;#txjwrdriwo p {
  margin: 0;
  padding: 0;
}
&#10;#txjwrdriwo .gt_table {
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
&#10;#txjwrdriwo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#txjwrdriwo .gt_title {
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
&#10;#txjwrdriwo .gt_subtitle {
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
&#10;#txjwrdriwo .gt_heading {
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
&#10;#txjwrdriwo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_col_headings {
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
&#10;#txjwrdriwo .gt_col_heading {
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
&#10;#txjwrdriwo .gt_column_spanner_outer {
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
&#10;#txjwrdriwo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#txjwrdriwo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#txjwrdriwo .gt_column_spanner {
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
&#10;#txjwrdriwo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#txjwrdriwo .gt_group_heading {
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
&#10;#txjwrdriwo .gt_empty_group_heading {
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
&#10;#txjwrdriwo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#txjwrdriwo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#txjwrdriwo .gt_row {
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
&#10;#txjwrdriwo .gt_stub {
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
&#10;#txjwrdriwo .gt_stub_row_group {
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
&#10;#txjwrdriwo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#txjwrdriwo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#txjwrdriwo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#txjwrdriwo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#txjwrdriwo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#txjwrdriwo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#txjwrdriwo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#txjwrdriwo .gt_footnotes {
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
&#10;#txjwrdriwo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#txjwrdriwo .gt_sourcenotes {
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
&#10;#txjwrdriwo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#txjwrdriwo .gt_left {
  text-align: left;
}
&#10;#txjwrdriwo .gt_center {
  text-align: center;
}
&#10;#txjwrdriwo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#txjwrdriwo .gt_font_normal {
  font-weight: normal;
}
&#10;#txjwrdriwo .gt_font_bold {
  font-weight: bold;
}
&#10;#txjwrdriwo .gt_font_italic {
  font-style: italic;
}
&#10;#txjwrdriwo .gt_super {
  font-size: 65%;
}
&#10;#txjwrdriwo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#txjwrdriwo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#txjwrdriwo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#txjwrdriwo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#txjwrdriwo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#txjwrdriwo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#txjwrdriwo .gt_indent_5 {
  text-indent: 25px;
}
&#10;#txjwrdriwo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#txjwrdriwo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Study 1: Number of unique IDs per condition |     |
|---------------------------------------------|-----|
| Reference Class                             | n   |
| Percentage                                  | 68  |
| USD                                         | 89  |
| kWh                                         | 78  |

</div>

<details class="code-fold">
<summary>Code</summary>

``` r
s1 |> group_by(refClass,state) |> 
  rename("Reference Class" = refClass) |>
  summarise(n=n_distinct(id)) |> 
  rowwise() |> 
  mutate(state=lookup_code(state)) |> 
  pivot_wider(names_from=state,values_from=n) |>
  gt() |> tab_header(title="Study 1: Number of unique IDs per condition by state") 
```

</details>

<div id="ytvoibwchg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ytvoibwchg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ytvoibwchg thead, #ytvoibwchg tbody, #ytvoibwchg tfoot, #ytvoibwchg tr, #ytvoibwchg td, #ytvoibwchg th {
  border-style: none;
}
&#10;#ytvoibwchg p {
  margin: 0;
  padding: 0;
}
&#10;#ytvoibwchg .gt_table {
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
&#10;#ytvoibwchg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ytvoibwchg .gt_title {
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
&#10;#ytvoibwchg .gt_subtitle {
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
&#10;#ytvoibwchg .gt_heading {
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
&#10;#ytvoibwchg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_col_headings {
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
&#10;#ytvoibwchg .gt_col_heading {
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
&#10;#ytvoibwchg .gt_column_spanner_outer {
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
&#10;#ytvoibwchg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ytvoibwchg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ytvoibwchg .gt_column_spanner {
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
&#10;#ytvoibwchg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ytvoibwchg .gt_group_heading {
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
&#10;#ytvoibwchg .gt_empty_group_heading {
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
&#10;#ytvoibwchg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ytvoibwchg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ytvoibwchg .gt_row {
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
&#10;#ytvoibwchg .gt_stub {
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
&#10;#ytvoibwchg .gt_stub_row_group {
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
&#10;#ytvoibwchg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ytvoibwchg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ytvoibwchg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytvoibwchg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ytvoibwchg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytvoibwchg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ytvoibwchg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytvoibwchg .gt_footnotes {
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
&#10;#ytvoibwchg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytvoibwchg .gt_sourcenotes {
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
&#10;#ytvoibwchg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytvoibwchg .gt_left {
  text-align: left;
}
&#10;#ytvoibwchg .gt_center {
  text-align: center;
}
&#10;#ytvoibwchg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ytvoibwchg .gt_font_normal {
  font-weight: normal;
}
&#10;#ytvoibwchg .gt_font_bold {
  font-weight: bold;
}
&#10;#ytvoibwchg .gt_font_italic {
  font-style: italic;
}
&#10;#ytvoibwchg .gt_super {
  font-size: 65%;
}
&#10;#ytvoibwchg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ytvoibwchg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ytvoibwchg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ytvoibwchg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ytvoibwchg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ytvoibwchg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ytvoibwchg .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ytvoibwchg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ytvoibwchg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Study 1: Number of unique IDs per condition by state |  |  |  |  |
|----|----|----|----|----|
| Reference Class | California | Colorado | Massachusetts | Texas |
| Percentage | 39 | 26 | 42 | 29 |
| USD | 40 | 45 | 44 | 49 |
| kWh | 36 | 36 | 42 | 42 |

</div>

<details class="code-fold">
<summary>Code</summary>

``` r
s1 |> group_by(refClass,calc) |> 
  rename("Reference Class" = refClass) |>
  summarise(n=n_distinct(id)) |> 
  pivot_wider(names_from=calc,values_from=n) |>
  gt() |> tab_header(title="Study 1: Number of unique IDs per condition by calculation")
```

</details>

<div id="utiozbhmrr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#utiozbhmrr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#utiozbhmrr thead, #utiozbhmrr tbody, #utiozbhmrr tfoot, #utiozbhmrr tr, #utiozbhmrr td, #utiozbhmrr th {
  border-style: none;
}
&#10;#utiozbhmrr p {
  margin: 0;
  padding: 0;
}
&#10;#utiozbhmrr .gt_table {
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
&#10;#utiozbhmrr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#utiozbhmrr .gt_title {
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
&#10;#utiozbhmrr .gt_subtitle {
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
&#10;#utiozbhmrr .gt_heading {
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
&#10;#utiozbhmrr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_col_headings {
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
&#10;#utiozbhmrr .gt_col_heading {
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
&#10;#utiozbhmrr .gt_column_spanner_outer {
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
&#10;#utiozbhmrr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#utiozbhmrr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#utiozbhmrr .gt_column_spanner {
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
&#10;#utiozbhmrr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#utiozbhmrr .gt_group_heading {
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
&#10;#utiozbhmrr .gt_empty_group_heading {
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
&#10;#utiozbhmrr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#utiozbhmrr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#utiozbhmrr .gt_row {
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
&#10;#utiozbhmrr .gt_stub {
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
&#10;#utiozbhmrr .gt_stub_row_group {
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
&#10;#utiozbhmrr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#utiozbhmrr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#utiozbhmrr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#utiozbhmrr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#utiozbhmrr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#utiozbhmrr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#utiozbhmrr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#utiozbhmrr .gt_footnotes {
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
&#10;#utiozbhmrr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#utiozbhmrr .gt_sourcenotes {
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
&#10;#utiozbhmrr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#utiozbhmrr .gt_left {
  text-align: left;
}
&#10;#utiozbhmrr .gt_center {
  text-align: center;
}
&#10;#utiozbhmrr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#utiozbhmrr .gt_font_normal {
  font-weight: normal;
}
&#10;#utiozbhmrr .gt_font_bold {
  font-weight: bold;
}
&#10;#utiozbhmrr .gt_font_italic {
  font-style: italic;
}
&#10;#utiozbhmrr .gt_super {
  font-size: 65%;
}
&#10;#utiozbhmrr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#utiozbhmrr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#utiozbhmrr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#utiozbhmrr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#utiozbhmrr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#utiozbhmrr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#utiozbhmrr .gt_indent_5 {
  text-indent: 25px;
}
&#10;#utiozbhmrr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#utiozbhmrr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Study 1: Number of unique IDs per condition by calculation |  |
|----|----|
| No Calculator | Used Calculator |
| Percentage |  |
| 12 | 56 |
| USD |  |
| 25 | 64 |
| kWh |  |
| 20 | 58 |

</div>

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass,s1)) |> kable()
```

</details>

| refClass   | Freq |
|:-----------|-----:|
| kWh        | 0.33 |
| Percentage | 0.29 |
| USD        | 0.38 |

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass+calc,s1)) |> kable()
```

</details>

|            | No Calculator | Used Calculator |
|:-----------|--------------:|----------------:|
| kWh        |          0.09 |            0.25 |
| Percentage |          0.05 |            0.24 |
| USD        |          0.11 |            0.27 |

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass+state,s1)) |> kable()
```

</details>

|            |  CAL |  COL | MASS |  TEX |
|:-----------|-----:|-----:|-----:|-----:|
| kWh        | 0.08 | 0.08 | 0.09 | 0.09 |
| Percentage | 0.08 | 0.06 | 0.09 | 0.06 |
| USD        | 0.09 | 0.10 | 0.09 | 0.10 |

### Study 2 - Subject counts

<details class="code-fold">
<summary>Code</summary>

``` r
s2_long |> group_by(refClass) |>
rename("Reference Class" = refClass) |>
summarise(n=n_distinct(id)) |> gt() |> tab_header(title="Study 2: Number of unique IDs per condition")
```

</details>

<div id="nxaeqswldi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nxaeqswldi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#nxaeqswldi thead, #nxaeqswldi tbody, #nxaeqswldi tfoot, #nxaeqswldi tr, #nxaeqswldi td, #nxaeqswldi th {
  border-style: none;
}
&#10;#nxaeqswldi p {
  margin: 0;
  padding: 0;
}
&#10;#nxaeqswldi .gt_table {
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
&#10;#nxaeqswldi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#nxaeqswldi .gt_title {
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
&#10;#nxaeqswldi .gt_subtitle {
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
&#10;#nxaeqswldi .gt_heading {
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
&#10;#nxaeqswldi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_col_headings {
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
&#10;#nxaeqswldi .gt_col_heading {
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
&#10;#nxaeqswldi .gt_column_spanner_outer {
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
&#10;#nxaeqswldi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#nxaeqswldi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#nxaeqswldi .gt_column_spanner {
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
&#10;#nxaeqswldi .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#nxaeqswldi .gt_group_heading {
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
&#10;#nxaeqswldi .gt_empty_group_heading {
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
&#10;#nxaeqswldi .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#nxaeqswldi .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#nxaeqswldi .gt_row {
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
&#10;#nxaeqswldi .gt_stub {
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
&#10;#nxaeqswldi .gt_stub_row_group {
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
&#10;#nxaeqswldi .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#nxaeqswldi .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#nxaeqswldi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nxaeqswldi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#nxaeqswldi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nxaeqswldi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#nxaeqswldi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nxaeqswldi .gt_footnotes {
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
&#10;#nxaeqswldi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nxaeqswldi .gt_sourcenotes {
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
&#10;#nxaeqswldi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nxaeqswldi .gt_left {
  text-align: left;
}
&#10;#nxaeqswldi .gt_center {
  text-align: center;
}
&#10;#nxaeqswldi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#nxaeqswldi .gt_font_normal {
  font-weight: normal;
}
&#10;#nxaeqswldi .gt_font_bold {
  font-weight: bold;
}
&#10;#nxaeqswldi .gt_font_italic {
  font-style: italic;
}
&#10;#nxaeqswldi .gt_super {
  font-size: 65%;
}
&#10;#nxaeqswldi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#nxaeqswldi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#nxaeqswldi .gt_indent_1 {
  text-indent: 5px;
}
&#10;#nxaeqswldi .gt_indent_2 {
  text-indent: 10px;
}
&#10;#nxaeqswldi .gt_indent_3 {
  text-indent: 15px;
}
&#10;#nxaeqswldi .gt_indent_4 {
  text-indent: 20px;
}
&#10;#nxaeqswldi .gt_indent_5 {
  text-indent: 25px;
}
&#10;#nxaeqswldi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#nxaeqswldi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Study 2: Number of unique IDs per condition |     |
|---------------------------------------------|-----|
| Reference Class                             | n   |
| Percentage                                  | 69  |
| USD                                         | 58  |
| kWh                                         | 69  |

</div>

<details class="code-fold">
<summary>Code</summary>

``` r
s2_long |> group_by(refClass,calc) |>
rename("Reference Class" = refClass) |>
mutate(calc=case_when(calc=="Calculator" ~ "Used Calculator",
                      calc=="No Calculator" ~ "No Calculator")) |>
summarise(n=n_distinct(id)) |>
pivot_wider(names_from=calc,values_from=n) |>
gt() |> tab_header(title="Study 2: Number of unique IDs per condition by calculation")
```

</details>

<div id="jqmmyzoclk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jqmmyzoclk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jqmmyzoclk thead, #jqmmyzoclk tbody, #jqmmyzoclk tfoot, #jqmmyzoclk tr, #jqmmyzoclk td, #jqmmyzoclk th {
  border-style: none;
}
&#10;#jqmmyzoclk p {
  margin: 0;
  padding: 0;
}
&#10;#jqmmyzoclk .gt_table {
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
&#10;#jqmmyzoclk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jqmmyzoclk .gt_title {
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
&#10;#jqmmyzoclk .gt_subtitle {
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
&#10;#jqmmyzoclk .gt_heading {
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
&#10;#jqmmyzoclk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_col_headings {
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
&#10;#jqmmyzoclk .gt_col_heading {
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
&#10;#jqmmyzoclk .gt_column_spanner_outer {
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
&#10;#jqmmyzoclk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jqmmyzoclk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jqmmyzoclk .gt_column_spanner {
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
&#10;#jqmmyzoclk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jqmmyzoclk .gt_group_heading {
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
&#10;#jqmmyzoclk .gt_empty_group_heading {
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
&#10;#jqmmyzoclk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jqmmyzoclk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jqmmyzoclk .gt_row {
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
&#10;#jqmmyzoclk .gt_stub {
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
&#10;#jqmmyzoclk .gt_stub_row_group {
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
&#10;#jqmmyzoclk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jqmmyzoclk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jqmmyzoclk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jqmmyzoclk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jqmmyzoclk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jqmmyzoclk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jqmmyzoclk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jqmmyzoclk .gt_footnotes {
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
&#10;#jqmmyzoclk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jqmmyzoclk .gt_sourcenotes {
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
&#10;#jqmmyzoclk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jqmmyzoclk .gt_left {
  text-align: left;
}
&#10;#jqmmyzoclk .gt_center {
  text-align: center;
}
&#10;#jqmmyzoclk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jqmmyzoclk .gt_font_normal {
  font-weight: normal;
}
&#10;#jqmmyzoclk .gt_font_bold {
  font-weight: bold;
}
&#10;#jqmmyzoclk .gt_font_italic {
  font-style: italic;
}
&#10;#jqmmyzoclk .gt_super {
  font-size: 65%;
}
&#10;#jqmmyzoclk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jqmmyzoclk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jqmmyzoclk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jqmmyzoclk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jqmmyzoclk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jqmmyzoclk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jqmmyzoclk .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jqmmyzoclk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jqmmyzoclk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Study 2: Number of unique IDs per condition by calculation |  |
|----|----|
| No Calculator | Used Calculator |
| Percentage |  |
| 11 | 58 |
| USD |  |
| 12 | 46 |
| kWh |  |
| 12 | 57 |

</div>

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass,s2_long)) |> kable()
```

</details>

| refClass   | Freq |
|:-----------|-----:|
| kWh        | 0.35 |
| Percentage | 0.35 |
| USD        | 0.30 |

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass+state,s2_long))
```

</details>

                state
    refClass     California Colorado Massachusetts Texas
      kWh             0.089    0.102         0.074 0.087
      Percentage      0.089    0.084         0.092 0.087
      USD             0.079    0.069         0.079 0.069

<details class="code-fold">
<summary>Code</summary>

``` r
prop.table(xtabs(~refClass+calc,
s2_long |> mutate(calc=case_when(calc=="Calculator" ~ "Used Calculator",
                      calc=="No Calculator" ~ "No Calculator")))) |> kable()
```

</details>

|            | No Calculator | Used Calculator |
|:-----------|--------------:|----------------:|
| kWh        |          0.06 |            0.29 |
| Percentage |          0.06 |            0.30 |
| USD        |          0.06 |            0.23 |

## Performance

### Study 1

<details class="code-fold">
<summary>Code</summary>

``` r
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
                abs_error = abs(error),
                close_match = abs_error <= 0.03)

s1_agg4 <- s1_agg |> group_by(id,refClass) |> 
    summarise(mg=sum(matched_goal),n=n(), pct=mg/n) 


# overall pct of subjects who matched their goal
s1_agg4 |> ungroup() |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 1: Proportion of participants who matched their goal overall")
```

</details>


    ---------------------------------
      mean      sd      n      se    
    -------- -------- ----- ---------
     0.2255   0.3814   235   0.02488 
    ---------------------------------

    Table: Study 1: Proportion of participants who matched their goal overall

<details class="code-fold">
<summary>Code</summary>

``` r
# proportion matching vs close match

s1_agg |> pivot_longer(c(matched_goal,close_match),names_to="match_type",values_to="match") |> 
    group_by(id,match_type) |>
    summarise(mg=sum(match),n=n(), pct=mg/n) |>
    group_by(match_type) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 1: Proportion of participants who matched their goal or were within 3% of their goal")
```

</details>


    ------------------------------------------------
      match_type     mean      sd      n      se    
    -------------- -------- -------- ----- ---------
     close_match    0.4149   0.4182   235   0.02728 

     matched_goal   0.2255   0.3814   235   0.02488 
    ------------------------------------------------

    Table: Study 1: Proportion of participants who matched their goal or were within 3% of their goal

<details class="code-fold">
<summary>Code</summary>

``` r
# refClass
s1_agg4 |> 
group_by(refClass) |> 
summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 1: Proportion of participants who matched their goal by condition")
```

</details>


    ----------------------------------------------
      refClass     mean       sd     n      se    
    ------------ --------- -------- ---- ---------
     Percentage   0.2206    0.3679   68   0.04461 

        USD       0.09831   0.2734   89   0.02898 

        kWh        0.375    0.4446   78   0.05034 
    ----------------------------------------------

    Table: Study 1: Proportion of participants who matched their goal by condition

### Study 2

<details class="code-fold">
<summary>Code</summary>

``` r
s2_agg1 <- s2_long |> 
  filter(appliance != "TOTAL") |> 
  group_by(id, state,refClass,pct,pct_goal,plan,rounded) |> 
  summarise(
    total_kWh = sum(value),
    orig_kWh = sum(family),
    pct_change = round((orig_kWh - total_kWh) / orig_kWh, 3),
    state_dif = mean(state_dif),
    .groups = "drop"
  ) |>
  mutate(matched_goal = (pct_change == pct),
  close_match = abs(pct_change-pct)<.03)

s2_agg4 <- s2_agg1|> group_by(id,refClass) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) 


# overall pct of subjects who matched their goal
s2_agg4 |> ungroup() |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal overall")
```

</details>


    ---------------------------------
      mean      sd      n      se    
    -------- -------- ----- ---------
     0.3023   0.4092   196   0.02923 
    ---------------------------------

    Table: Study 2: Proportion of participants who matched their goal overall

<details class="code-fold">
<summary>Code</summary>

``` r
# proportion matching vs close match

s2_agg1 |> pivot_longer(c(matched_goal,close_match),names_to="match_type",values_to="match") |> 
    group_by(id,match_type) |>
    summarise(mg=sum(match),n=n(), pct=mg/n) |>
    group_by(match_type) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal or were within 3% of their goal")
```

</details>


    ------------------------------------------------
      match_type     mean      sd      n      se    
    -------------- -------- -------- ----- ---------
     close_match    0.4273   0.438    196   0.03128 

     matched_goal   0.3023   0.4092   196   0.02923 
    ------------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal or were within 3% of their goal

<details class="code-fold">
<summary>Code</summary>

``` r
# refClass only 
s2_agg4 |> 
    group_by(refClass) |> 
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by condition")
```

</details>


    ---------------------------------------------
      refClass     mean      sd     n      se    
    ------------ -------- -------- ---- ---------
     Percentage   0.2681   0.3621   69   0.04359 

        USD       0.1853   0.3763   58   0.04941 

        kWh       0.4348   0.4469   69   0.0538  
    ---------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by condition

<details class="code-fold">
<summary>Code</summary>

``` r
# rounded only

s2_agg1 |> group_by(id,rounded) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(rounded) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by rounding condition")
```

</details>


    -----------------------------------------------
       rounded      mean      sd      n      se    
    ------------- -------- -------- ----- ---------
     Not Rounded   0.2653   0.4219   196   0.03013 

       Rounded     0.3393   0.454    196   0.03243 
    -----------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by rounding condition

<details class="code-fold">
<summary>Code</summary>

``` r
# pct_goal only

s2_agg1 |> group_by(id,pct_goal) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(pct_goal) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by goal condition")
```

</details>


    --------------------------------------------
     pct_goal    mean      sd      n      se    
    ---------- -------- -------- ----- ---------
       10%      0.2959   0.4405   196   0.03146 

       15%      0.3087   0.439    196   0.03135 
    --------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by goal condition

<details class="code-fold">
<summary>Code</summary>

``` r
# refClass x rounded

s2_agg1 |> group_by(id, refClass,rounded) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(refClass,rounded) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by condition and rounding")
```

</details>


    -----------------------------------------------------------
      refClass      rounded      mean      sd     n      se    
    ------------ ------------- -------- -------- ---- ---------
     Percentage   Not Rounded   0.1957   0.3856   69   0.04642 

     Percentage     Rounded     0.3406   0.4496   69   0.05413 

        USD       Not Rounded   0.1897   0.3842   58   0.05045 

        USD         Rounded     0.181    0.3827   58   0.05025 

        kWh       Not Rounded   0.3986   0.4584   69   0.05519 

        kWh         Rounded     0.471    0.4765   69   0.05737 
    -----------------------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by condition and rounding

<details class="code-fold">
<summary>Code</summary>

``` r
# refClass x pct_goal

s2_agg1 |> group_by(id, refClass,pct_goal) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(refClass,pct_goal) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by condition and goal")
```

</details>


    --------------------------------------------------------
      refClass    pct_goal    mean      sd     n      se    
    ------------ ---------- -------- -------- ---- ---------
     Percentage     10%      0.2536   0.4168   69   0.05017 

     Percentage     15%      0.2826   0.4328   69   0.05211 

        USD         10%      0.1724   0.3693   58   0.0485  

        USD         15%      0.1983   0.3967   58   0.05209 

        kWh         10%      0.442    0.4815   69   0.05797 

        kWh         15%      0.4275   0.456    69   0.05489 
    --------------------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by condition and goal

<details class="code-fold">
<summary>Code</summary>

``` r
# pct_goal x rounded

s2_agg1 |> group_by(id, pct_goal,rounded) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(pct_goal,rounded) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by goal and rounding")
```

</details>


    ----------------------------------------------------------
     pct_goal     rounded      mean      sd      n      se    
    ---------- ------------- -------- -------- ----- ---------
       10%      Not Rounded   0.2188   0.3961   96    0.04043 

       10%        Rounded      0.37    0.4694   100   0.04694 

       15%      Not Rounded    0.31    0.4426   100   0.04426 

       15%        Rounded     0.3073   0.4375   96    0.04465 
    ----------------------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by goal and rounding

<details class="code-fold">
<summary>Code</summary>

``` r
# pct_goal x rounded x refClass

s2_agg1 |> group_by(id, pct_goal,rounded,refClass) |> 
  summarise(mg=sum(matched_goal),n=n(), pct=mg/n) |> 
    group_by(pct_goal,rounded,refClass) |>
    summarise(
    mean = mean(pct),
    sd = sd(pct),
    n = n(),
    se=sd(pct)/sqrt(n)
) |> pander::pandoc.table(caption="Study 2: Proportion of participants who matched their goal by goal, rounding, and condition")
```

</details>


    ----------------------------------------------------------------------
     pct_goal     rounded      refClass     mean      sd     n      se    
    ---------- ------------- ------------ -------- -------- ---- ---------
       10%      Not Rounded   Percentage   0.1111   0.2955   36   0.04924 

       10%      Not Rounded      USD       0.1333   0.3198   30   0.05839 

       10%      Not Rounded      kWh       0.4333   0.4866   30   0.08884 

       10%        Rounded     Percentage   0.4091   0.4752   33   0.08273 

       10%        Rounded        USD       0.2143   0.4179   28   0.07897 

       10%        Rounded        kWh       0.4487   0.4839   39   0.07748 

       15%      Not Rounded   Percentage   0.2879   0.4512   33   0.07855 

       15%      Not Rounded      USD        0.25    0.441    28   0.08333 

       15%      Not Rounded      kWh       0.3718   0.4401   39   0.07047 

       15%        Rounded     Percentage   0.2778   0.4216   36   0.07027 

       15%        Rounded        USD        0.15    0.3511   30   0.0641  

       15%        Rounded        kWh        0.5     0.4734   30   0.08644 
    ----------------------------------------------------------------------

    Table: Study 2: Proportion of participants who matched their goal by goal, rounding, and condition

## Analysis

### Study 1

<details class="code-fold">
<summary>Code</summary>

``` r
# binomial regression on trial level data

s1_br <- brm(
    matched_goal ~ refClass + (1|id),
    data = s1_agg,
    family = bernoulli(),
    cores=4,
    iter=2000,
    control = list(adapt_delta = 0.99),
    file=paste0(here::here("data/model_cache",'s1_br.rds')),
    )

summary(s1_br)
```

</details>

     Family: bernoulli 
      Links: mu = logit 
    Formula: matched_goal ~ refClass + (1 | id) 
       Data: s1_agg (Number of observations: 940) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Multilevel Hyperparameters:
    ~id (Number of levels: 235) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     7.19      1.18     5.22     9.83 1.00     1100     1777

    Regression Coefficients:
                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept             -2.31      1.05    -4.59    -0.41 1.00      851     1367
    refClassPercentage    -3.16      1.60    -6.49    -0.13 1.00      721     1194
    refClassUSD           -7.25      1.90   -11.50    -3.92 1.00      895     1244

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

<details class="code-fold">
<summary>Code</summary>

``` r
mr_s1 <- as.data.frame(describe_posterior(s1_br, centrality = "Mean"))[, c(1,2,4,5,6)]
colnames(mr_s1) <- c("Term", "Estimate","95% CrI Lower", "95% CrI Upper", "pd")

intercept <- mr_s1$Estimate[mr_s1$Term == "b_Intercept"]

 mr_s1 |>
    mutate(Term = stringr::str_remove(Term, "b_")) |>
    mutate(across(c("Estimate", "95% CrI Lower", "95% CrI Upper"), 
                    \(x) if_else(Term == "Intercept", plogis(x), plogis(x + intercept)))) |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    mutate(Term = if_else(Term == "Intercept", "Intercept (kWh)", Term)) |>
    tibble::remove_rownames() |>
    kable(booktabs = TRUE)
```

</details>

| Term               | Estimate | 95% CrI Lower | 95% CrI Upper |   pd |
|:-------------------|---------:|--------------:|--------------:|-----:|
| Intercept (kWh)    |     0.09 |          0.01 |          0.40 | 0.99 |
| refClassPercentage |     0.00 |          0.00 |          0.08 | 0.98 |
| refClassUSD        |     0.00 |          0.00 |          0.00 | 1.00 |

<details class="code-fold">
<summary>Code</summary>

``` r
s1_br2 <- brm(
    close_match ~ refClass + (1|id) + (1|state),
    data = s1_agg,
    family = bernoulli(),
    cores=4,
    iter=2000,
    control = list(adapt_delta = 0.99),
    file=paste0(here::here("data/model_cache",'s1_br2.rds')),
    )
summary(s1_br2)
```

</details>

     Family: bernoulli 
      Links: mu = logit 
    Formula: close_match ~ refClass + (1 | id) + (1 | state) 
       Data: s1_agg (Number of observations: 940) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Multilevel Hyperparameters:
    ~id (Number of levels: 235) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     3.88      0.43     3.12     4.79 1.00      963     2098

    ~state (Number of levels: 4) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.42      0.44     0.02     1.51 1.00     1494     2007

    Regression Coefficients:
                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept              0.62      0.59    -0.55     1.80 1.01     1001     1167
    refClassPercentage    -0.90      0.76    -2.43     0.49 1.01      737     1299
    refClassUSD           -3.17      0.78    -4.80    -1.70 1.01     1083     1559

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

<details class="code-fold">
<summary>Code</summary>

``` r
s1_br2 |> emmeans(~refClass, type="response")
```

</details>

     refClass   response lower.HPD upper.HPD
     kWh            0.65      0.40      0.89
     Percentage     0.43      0.17      0.69
     USD            0.07      0.01      0.17

    Point estimate displayed: median 
    Results are back-transformed from the logit scale 
    HPD interval probability: 0.95 

### Study 2

<details class="code-fold">
<summary>Code</summary>

``` r
# binomial regression on trial level data

s2_br_3 <- brm(
    matched_goal ~ refClass+rounded+pct_goal + (1|id) + (1|state),
    data = s2_agg1,
    family = bernoulli(),
    cores=4,
    iter=2000,
    control = list(adapt_delta = 0.99),
    file=paste0(here::here("data/model_cache",'s2_br_3.rds')),
    )

summary(s2_br_3)
```

</details>

     Family: bernoulli 
      Links: mu = logit 
    Formula: matched_goal ~ refClass + rounded + pct_goal + (1 | id) + (1 | state) 
       Data: s2_agg1 (Number of observations: 784) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Multilevel Hyperparameters:
    ~id (Number of levels: 196) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     6.98      1.13     5.06     9.49 1.00      844     1813

    ~state (Number of levels: 4) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.76      0.70     0.03     2.66 1.00     1280     1786

    Regression Coefficients:
                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept             -1.95      1.19    -4.38     0.34 1.00      871     1513
    refClassPercentage    -3.03      1.46    -6.14    -0.28 1.01      580      868
    refClassUSD           -5.67      1.79    -9.40    -2.37 1.00      741     1301
    roundedRounded         1.51      0.37     0.81     2.24 1.00     4056     3115
    pct_goal15%            0.27      0.38    -0.45     1.03 1.00     3990     2550

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

<details class="code-fold">
<summary>Code</summary>

``` r
mr_s2_3 <- as.data.frame(describe_posterior(s2_br_3, centrality = "Mean"))[, c(1,2,4,5,6)]
colnames(mr_s2_3) <- c("Term", "Estimate","95% CrI Lower", "95% CrI Upper", "pd")


 mr_s2_3 |>
    mutate(Term = stringr::str_remove(Term, "b_")) |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    mutate(Term = if_else(Term == "Intercept", "Intercept (kWh)", Term)) |>
    tibble::remove_rownames() |>
    kable(booktabs = TRUE)
```

</details>

| Term               | Estimate | 95% CrI Lower | 95% CrI Upper |   pd |
|:-------------------|---------:|--------------:|--------------:|-----:|
| Intercept (kWh)    |    -1.96 |         -4.38 |          0.34 | 0.96 |
| refClassPercentage |    -3.03 |         -6.14 |         -0.28 | 0.99 |
| refClassUSD        |    -5.67 |         -9.40 |         -2.37 | 1.00 |
| roundedRounded     |     1.50 |          0.81 |          2.24 | 1.00 |
| pct_goal15%        |     0.27 |         -0.45 |          1.03 | 0.76 |

<details class="code-fold">
<summary>Code</summary>

``` r
s2_br2_3 <- brm(
    close_match ~ refClass+rounded+pct_goal + (1|id) + (1|state),
    data = s2_agg1,
    family = bernoulli(),
    cores=4,
    iter=2000,
    control = list(adapt_delta = 0.99),
    file=paste0(here::here("data/model_cache",'s2_br2_3.rds')),
    )

summary(s2_br2_3)
```

</details>

     Family: bernoulli 
      Links: mu = logit 
    Formula: close_match ~ refClass + rounded + pct_goal + (1 | id) + (1 | state) 
       Data: s2_agg1 (Number of observations: 784) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Multilevel Hyperparameters:
    ~id (Number of levels: 196) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     5.80      0.82     4.37     7.65 1.00     1100     2329

    ~state (Number of levels: 4) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.73      0.63     0.03     2.42 1.00     1114     1439

    Regression Coefficients:
                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept              0.09      0.94    -1.76     2.01 1.00     1032     1599
    refClassPercentage    -1.64      1.16    -3.95     0.57 1.00      881     1466
    refClassUSD           -3.55      1.29    -6.17    -1.13 1.00      716     1520
    roundedRounded         0.56      0.29     0.00     1.12 1.00     5055     3112
    pct_goal15%            0.66      0.29     0.11     1.26 1.00     5026     2940

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

<details class="code-fold">
<summary>Code</summary>

``` r
#s2_br2_3 |> emmeans(~refClass, type="response")

mr_s2_3_2 <- as.data.frame(describe_posterior(s2_br2_3, centrality = "Mean"))[, c(1,2,4,5,6)]
colnames(mr_s2_3_2) <- c("Term", "Estimate","95% CrI Lower", "95% CrI Upper", "pd")


mr_s2_3_2 |>
    mutate(Term = stringr::str_remove(Term, "b_")) |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    mutate(Term = if_else(Term == "Intercept", "Intercept (kWh)", Term)) |>
    tibble::remove_rownames() |>
    kable(booktabs = TRUE)
```

</details>

| Term               | Estimate | 95% CrI Lower | 95% CrI Upper |   pd |
|:-------------------|---------:|--------------:|--------------:|-----:|
| Intercept (kWh)    |     0.09 |          -1.8 |          2.01 | 0.53 |
| refClassPercentage |    -1.64 |          -4.0 |          0.57 | 0.93 |
| refClassUSD        |    -3.55 |          -6.2 |         -1.13 | 1.00 |
| roundedRounded     |     0.56 |           0.0 |          1.12 | 0.98 |
| pct_goal15%        |     0.66 |           0.1 |          1.26 | 0.99 |
