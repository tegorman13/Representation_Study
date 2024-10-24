# Study 2 Analysis
Thomas E. Gorman

    # A tibble: 3 × 4
      refClass   mean_pct sd_pct     n
      <chr>         <dbl>  <dbl> <int>
    1 Percentage    0.26   0.345    75
    2 USD           0.179  0.371    60
    3 kWh           0.423  0.446    71

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

![](study2_files/figure-commonmark/unnamed-chunk-4-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-4-2.png)

## Inspect response distribution for outliers

    Skewness: 1.2 

    Kurtosis: 3 


        Shapiro-Wilk normality test

    data:  s2_agg$pct_change[1:5000]
    W = 0.8, p-value <0.0000000000000002

<div class="column-page-right">

## Before and after excluding outliers

![](study2_files/figure-commonmark/unnamed-chunk-6-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-6-2.png)

## Individual Participant responses

- Facets indicate the Reference class (%, kWh, USD) and the goal %
  reduction (10%, 15%)
- dashed lines also indicate goal percentage - dots on the line indicate
  participants who met the goal

![](study2_files/figure-commonmark/unnamed-chunk-7-1.png)

## Interactive data table

![](study2_files/figure-commonmark/unnamed-chunk-8-1.png)

</div>

## Looking at % of subjects who hit the target reduction

![](study2_files/figure-commonmark/unnamed-chunk-9-1.png)

<div id="mozfapwklt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mozfapwklt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mozfapwklt thead, #mozfapwklt tbody, #mozfapwklt tfoot, #mozfapwklt tr, #mozfapwklt td, #mozfapwklt th {
  border-style: none;
}
&#10;#mozfapwklt p {
  margin: 0;
  padding: 0;
}
&#10;#mozfapwklt .gt_table {
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
&#10;#mozfapwklt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mozfapwklt .gt_title {
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
&#10;#mozfapwklt .gt_subtitle {
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
&#10;#mozfapwklt .gt_heading {
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
&#10;#mozfapwklt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_col_headings {
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
&#10;#mozfapwklt .gt_col_heading {
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
&#10;#mozfapwklt .gt_column_spanner_outer {
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
&#10;#mozfapwklt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mozfapwklt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mozfapwklt .gt_column_spanner {
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
&#10;#mozfapwklt .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mozfapwklt .gt_group_heading {
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
&#10;#mozfapwklt .gt_empty_group_heading {
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
&#10;#mozfapwklt .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mozfapwklt .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mozfapwklt .gt_row {
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
&#10;#mozfapwklt .gt_stub {
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
&#10;#mozfapwklt .gt_stub_row_group {
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
&#10;#mozfapwklt .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mozfapwklt .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mozfapwklt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mozfapwklt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mozfapwklt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mozfapwklt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mozfapwklt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mozfapwklt .gt_footnotes {
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
&#10;#mozfapwklt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mozfapwklt .gt_sourcenotes {
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
&#10;#mozfapwklt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mozfapwklt .gt_left {
  text-align: left;
}
&#10;#mozfapwklt .gt_center {
  text-align: center;
}
&#10;#mozfapwklt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mozfapwklt .gt_font_normal {
  font-weight: normal;
}
&#10;#mozfapwklt .gt_font_bold {
  font-weight: bold;
}
&#10;#mozfapwklt .gt_font_italic {
  font-style: italic;
}
&#10;#mozfapwklt .gt_super {
  font-size: 65%;
}
&#10;#mozfapwklt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mozfapwklt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mozfapwklt .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mozfapwklt .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mozfapwklt .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mozfapwklt .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mozfapwklt .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mozfapwklt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mozfapwklt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" style="width:100%;"
data-quarto-postprocess="true" data-quarto-disable-processing="false"
data-quarto-bootstrap="false">
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
<th colspan="6"
class="gt_heading gt_title gt_font_normal"><strong>Percentage of
Participants Matching Target Pct by Condition</strong></th>
</tr>
<tr class="gt_heading">
<th colspan="6"
class="gt_heading gt_subtitle gt_font_normal gt_bottom_border">Grouped
by Reference Class, Rounding, and Pct Goal</th>
</tr>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="refClass"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Reference Class</th>
<th rowspan="2" id="rounded"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Rounding</th>
<th rowspan="2" id="pct_goal"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Pct Goal</th>
<th colspan="3" id="Participants"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Participants
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="matched_count"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Matched Count</th>
<th id="total_count"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Total Count</th>
<th id="pct_matched"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Percentage Matched (%)</th>
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

![](study2_files/figure-commonmark/unnamed-chunk-10-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-10-2.png)

## Cumulative plot

**Click on plots to enlarge**

![](study2_files/figure-commonmark/unnamed-chunk-11-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-11-2.png)

![](study2_files/figure-commonmark/unnamed-chunk-11-3.png)

![](study2_files/figure-commonmark/unnamed-chunk-11-4.png)

## Main Effects and Interactions

**Click on plots to enlarge**

![](study2_files/figure-commonmark/unnamed-chunk-12-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-12-2.png)

![](study2_files/figure-commonmark/unnamed-chunk-12-3.png)

![](study2_files/figure-commonmark/unnamed-chunk-12-4.png)

![](study2_files/figure-commonmark/unnamed-chunk-12-5.png)

![](study2_files/figure-commonmark/unnamed-chunk-12-6.png)

## Calculator and education

| edu                       |   n |
|:--------------------------|----:|
| College degree            | 104 |
| Graduate degree           |  51 |
| Highschool diploma or GED |  11 |
| Some college              |  32 |
| Some graduate school      |   9 |

Count of Items by Education Level

| calc            |   n |
|:----------------|----:|
| No Calculator   |  39 |
| Used Calculator | 168 |

Count of Items by Calculator Use

<div id="jgrypichtp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jgrypichtp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jgrypichtp thead, #jgrypichtp tbody, #jgrypichtp tfoot, #jgrypichtp tr, #jgrypichtp td, #jgrypichtp th {
  border-style: none;
}
&#10;#jgrypichtp p {
  margin: 0;
  padding: 0;
}
&#10;#jgrypichtp .gt_table {
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
&#10;#jgrypichtp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jgrypichtp .gt_title {
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
&#10;#jgrypichtp .gt_subtitle {
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
&#10;#jgrypichtp .gt_heading {
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
&#10;#jgrypichtp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_col_headings {
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
&#10;#jgrypichtp .gt_col_heading {
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
&#10;#jgrypichtp .gt_column_spanner_outer {
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
&#10;#jgrypichtp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jgrypichtp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jgrypichtp .gt_column_spanner {
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
&#10;#jgrypichtp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jgrypichtp .gt_group_heading {
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
&#10;#jgrypichtp .gt_empty_group_heading {
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
&#10;#jgrypichtp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jgrypichtp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jgrypichtp .gt_row {
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
&#10;#jgrypichtp .gt_stub {
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
&#10;#jgrypichtp .gt_stub_row_group {
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
&#10;#jgrypichtp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jgrypichtp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jgrypichtp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jgrypichtp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jgrypichtp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jgrypichtp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jgrypichtp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jgrypichtp .gt_footnotes {
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
&#10;#jgrypichtp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jgrypichtp .gt_sourcenotes {
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
&#10;#jgrypichtp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jgrypichtp .gt_left {
  text-align: left;
}
&#10;#jgrypichtp .gt_center {
  text-align: center;
}
&#10;#jgrypichtp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jgrypichtp .gt_font_normal {
  font-weight: normal;
}
&#10;#jgrypichtp .gt_font_bold {
  font-weight: bold;
}
&#10;#jgrypichtp .gt_font_italic {
  font-style: italic;
}
&#10;#jgrypichtp .gt_super {
  font-size: 65%;
}
&#10;#jgrypichtp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jgrypichtp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jgrypichtp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jgrypichtp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jgrypichtp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jgrypichtp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jgrypichtp .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jgrypichtp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jgrypichtp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="gt_heading">
<th colspan="5"
class="gt_heading gt_title gt_font_normal gt_bottom_border"><strong>Count
of Items by Calculator Use, Reference Class, and Rounding</strong></th>
</tr>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="refClass"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">refClass</th>
<th colspan="2" id="spanner-No Calculator_Not Rounded"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
No Calculator
</div></th>
<th colspan="2" id="spanner-Used Calculator_Not Rounded"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Used Calculator
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="No-Calculator_Not-Rounded"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Not Rounded</th>
<th id="No-Calculator_Rounded"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Rounded</th>
<th id="Used-Calculator_Not-Rounded"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Not Rounded</th>
<th id="Used-Calculator_Rounded"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Rounded</th>
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

<div id="dbtmpwlsjw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dbtmpwlsjw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dbtmpwlsjw thead, #dbtmpwlsjw tbody, #dbtmpwlsjw tfoot, #dbtmpwlsjw tr, #dbtmpwlsjw td, #dbtmpwlsjw th {
  border-style: none;
}
&#10;#dbtmpwlsjw p {
  margin: 0;
  padding: 0;
}
&#10;#dbtmpwlsjw .gt_table {
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
&#10;#dbtmpwlsjw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dbtmpwlsjw .gt_title {
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
&#10;#dbtmpwlsjw .gt_subtitle {
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
&#10;#dbtmpwlsjw .gt_heading {
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
&#10;#dbtmpwlsjw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_col_headings {
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
&#10;#dbtmpwlsjw .gt_col_heading {
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
&#10;#dbtmpwlsjw .gt_column_spanner_outer {
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
&#10;#dbtmpwlsjw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dbtmpwlsjw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dbtmpwlsjw .gt_column_spanner {
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
&#10;#dbtmpwlsjw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dbtmpwlsjw .gt_group_heading {
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
&#10;#dbtmpwlsjw .gt_empty_group_heading {
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
&#10;#dbtmpwlsjw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dbtmpwlsjw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dbtmpwlsjw .gt_row {
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
&#10;#dbtmpwlsjw .gt_stub {
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
&#10;#dbtmpwlsjw .gt_stub_row_group {
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
&#10;#dbtmpwlsjw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dbtmpwlsjw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dbtmpwlsjw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dbtmpwlsjw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dbtmpwlsjw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dbtmpwlsjw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dbtmpwlsjw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dbtmpwlsjw .gt_footnotes {
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
&#10;#dbtmpwlsjw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dbtmpwlsjw .gt_sourcenotes {
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
&#10;#dbtmpwlsjw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dbtmpwlsjw .gt_left {
  text-align: left;
}
&#10;#dbtmpwlsjw .gt_center {
  text-align: center;
}
&#10;#dbtmpwlsjw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dbtmpwlsjw .gt_font_normal {
  font-weight: normal;
}
&#10;#dbtmpwlsjw .gt_font_bold {
  font-weight: bold;
}
&#10;#dbtmpwlsjw .gt_font_italic {
  font-style: italic;
}
&#10;#dbtmpwlsjw .gt_super {
  font-size: 65%;
}
&#10;#dbtmpwlsjw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dbtmpwlsjw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dbtmpwlsjw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dbtmpwlsjw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dbtmpwlsjw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dbtmpwlsjw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dbtmpwlsjw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#dbtmpwlsjw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#dbtmpwlsjw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

<div id="shszhpwlvj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#shszhpwlvj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#shszhpwlvj thead, #shszhpwlvj tbody, #shszhpwlvj tfoot, #shszhpwlvj tr, #shszhpwlvj td, #shszhpwlvj th {
  border-style: none;
}
&#10;#shszhpwlvj p {
  margin: 0;
  padding: 0;
}
&#10;#shszhpwlvj .gt_table {
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
&#10;#shszhpwlvj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#shszhpwlvj .gt_title {
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
&#10;#shszhpwlvj .gt_subtitle {
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
&#10;#shszhpwlvj .gt_heading {
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
&#10;#shszhpwlvj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_col_headings {
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
&#10;#shszhpwlvj .gt_col_heading {
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
&#10;#shszhpwlvj .gt_column_spanner_outer {
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
&#10;#shszhpwlvj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#shszhpwlvj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#shszhpwlvj .gt_column_spanner {
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
&#10;#shszhpwlvj .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#shszhpwlvj .gt_group_heading {
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
&#10;#shszhpwlvj .gt_empty_group_heading {
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
&#10;#shszhpwlvj .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#shszhpwlvj .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#shszhpwlvj .gt_row {
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
&#10;#shszhpwlvj .gt_stub {
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
&#10;#shszhpwlvj .gt_stub_row_group {
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
&#10;#shszhpwlvj .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#shszhpwlvj .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#shszhpwlvj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#shszhpwlvj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#shszhpwlvj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#shszhpwlvj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#shszhpwlvj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#shszhpwlvj .gt_footnotes {
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
&#10;#shszhpwlvj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#shszhpwlvj .gt_sourcenotes {
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
&#10;#shszhpwlvj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#shszhpwlvj .gt_left {
  text-align: left;
}
&#10;#shszhpwlvj .gt_center {
  text-align: center;
}
&#10;#shszhpwlvj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#shszhpwlvj .gt_font_normal {
  font-weight: normal;
}
&#10;#shszhpwlvj .gt_font_bold {
  font-weight: bold;
}
&#10;#shszhpwlvj .gt_font_italic {
  font-style: italic;
}
&#10;#shszhpwlvj .gt_super {
  font-size: 65%;
}
&#10;#shszhpwlvj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#shszhpwlvj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#shszhpwlvj .gt_indent_1 {
  text-indent: 5px;
}
&#10;#shszhpwlvj .gt_indent_2 {
  text-indent: 10px;
}
&#10;#shszhpwlvj .gt_indent_3 {
  text-indent: 15px;
}
&#10;#shszhpwlvj .gt_indent_4 {
  text-indent: 20px;
}
&#10;#shszhpwlvj .gt_indent_5 {
  text-indent: 25px;
}
&#10;#shszhpwlvj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#shszhpwlvj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| <strong>Count of Items by Calculator Use and Education Level</strong> |  |  |  |  |  |
|----|----|----|----|----|----|
|  | College degree | Graduate degree | Highschool diploma or GED | Some college | Some graduate school |
| No Calculator | 21 | 11 | 1 | 4 | 2 |
| Used Calculator | 83 | 40 | 10 | 28 | 7 |

</div>

![](study2_files/figure-commonmark/unnamed-chunk-13-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-13-2.png)

![](study2_files/figure-commonmark/unnamed-chunk-14-1.png)

![](study2_files/figure-commonmark/unnamed-chunk-14-2.png)
