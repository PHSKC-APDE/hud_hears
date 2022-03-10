# **Associations Between Public Housing Exit Type and Long-Term Outcomes**

Code developed by UW Biostatistics Capstone students in collaboration with Public Health - Seattle and King County on the HUD HEARS project.

(TO DO: Insert general overview here after discussing with team. Not sure what else to include. Probably names/contact information?)

**Background:** The lack of affordable housing is a known social determinant of health. For those who cannot afford housing, public housing is often an option, however it is a limited resource. Because of this, it is more sustainable for public housing authorities to help people successfully exit from public housing in order to create space for others in need. Since previous research is sparse in understanding public housing exits and their potential associations with long-term outcomes, this project aims to fill this gap by investigating the relationship between exit types and housing stability, defined by time to experiencing homelessness within 1 year of exiting from public housing.

**Study Design:** This is a retrospective cohort study that includes 17,224 individuals who received and exited from federal housing assistance at either (a). Seattle Housing Authority (SHA) between 01/01/2012 and 12/31/2018, or (b). King County Housing Authority (KCHA) between 01/01/2016 and 12/31/2018. Each individual has exactly one exit which is either positive, neutral, or negative. The housing outcome of individuals with positive or negative exits are compared to those of individuals with neutral exits. 

**Statistical Methods:** First, to investigate the relationship between exit type and time-to-homelessness, Kaplan-Meier survival curves are fit for each exit type in order to show the probability of remaining housed for those who experience positive exits, neutral exits, or negative exits. Additionally, to explore this relationship while also taking into account potential confounding, propensity scores are calculated via a multinomial logistic regression model, with confounding individual-, household-, and neighborhood-level variables as the predictors and exit type as the outcome. Generalized estimating equations are also used with clustering by household, as multiple individuals in the data may belong to the same household, and hence share attributes. These propensity scores are then used to calculate weights via inverse probability treatment weighting. Then, these weights are used as input when fitting a Cox proportional hazards model, where the predictor of interest is exit type and the outcome is time to homelessness. Here, clustering by household via a robust sandwich estimator is also included. Hazard ratios are then interpreted. Moreover, a leave-one-out sensitivity analysis is performed in order to determine which specific exit reasons are most influential, and whether the primary analysis results are robust to any one particular exit reason. This sensitivity analysis is carried out by removing each exit reason one at a time, and then re-running the primary analysis. Two forest plots are analyzed (first comparing negative exits to neutral exits, then comparing positive exits to neutral exits) in order to draw conclusions.

**Results:** Based on the Kaplan-Meier curves, the probability of remaining housed for those with positive exits is approximately 97%. The probability of remaining housed for those with neutral exits is approximately 85%. The probability of remaining housed for those with negative exits is approximately 75%. Additionally, based on the Cox proportional hazards model, the hazard of experiencing homelessness for people with positive exits from public housing is approximately 19% (95% CI: 14%, 26%) of the hazard for people with neutral exits from public housing. The hazard of experiencing homelessness for people with negative exits from public housing is approximately 87% (95% CI: 67%, 109%) greater than the hazard for people with neutral exits from public housing. From the sensitivity analysis forest plots, it is shown that the primary analysis results are robust to any particular exit reason. Additionally, the neutral-classified exit reason, “PB/MR moved out location unknown” appears to be more negative in nature, and the exit reason “Landlord eviction” appears to be the most influential negative exit reason.

**Discussion:** Positive exits are generally associated with a lower probability of experiencing homelessness within the first year post-exit, and negative exits are generally associated with a higher probability of experiencing homelessness within the first year post-exit. Since how someone exits from public housing has been shown to be associated with housing stability, this provides further evidence for public housing authorities to allocate resources towards helping people achieve positive exits and avoid negative exits from public housing. Some next steps include exploring other long-term outcomes, such as change in income, and further investigating the impact that specific exit reasons have on long-term outcomes.

Written by: Hantong Hu, Taylor Keating, Zichen Liu, Niki Petrakos


## Summary

1.  If the Opportunity Index data does not need to be updated, **skip 00_opportunity_index**.
2.  **Run 01_format.R** to query and format the data before performing analyses.
3.  **Run any set of scripts in the 02_results folder** depending on which analyses are desired.

## Contents

[00_opportunity_index](#00_opportunity_index)

[01_format](#01_format)

[02_results](#02_results)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[01_descriptive_stats](#01_descriptive_stats)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[02_km_curve](#02_km_curve)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[03_regression](#03_regression)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[04_exit_reason_sensitivity](#04_exit_reason_sensitivity)

## 00_opportunity_index

**Usage:** Ignore this step unless Opportunity Index is updated

**Output:** `kc_opp_indices_scaled.csv`

**Details:**

`create_opportunity_index.R` is used to create `kc_opp_indices_scaled.csv`from `PSRC_opp_mapping_all_indicators_raw.xlsx`. This has already been done using PSRC's 2018 Opportunity Index. Thus, unless the Opportunity Index data is updated, **this script does not need to be re-run prior to formatting or analysis** since `kc_opp_indices_scaled.csv` is ready to be used.

`kc_opp_indices_scaled.csv` is used in the `01_format.R` script to append Opportunity Index data onto the public housing data. Please keep this file in its original directory to ensure the script is able to locate and read it.

For a more thorough description of the contents of this folder and how the script works, please read the extended documentation [here](https://github.com/PHSKC-APDE/hud_hears/blob/main/analyses/capstone/00_opportunity_index/README.md).

## 01_format

**Usage:** This script must be run from start to finish after `kc_opp_indices_scaled.csv` is created in the `00_opportunity_index` folder but before any analysis scripts in the `02_results` folder can be run. A KingCounty.gov username and password are required to perform database queries. Several required R packages may be installed by the `p_load` function upon initial use.

**Output:** Two intermediate tables for debugging and a final table, `hudhears.capstone_data_3` ready for analysis scripts in `02_results` folder.

**Details:**

The purpose of this script is threefold:

1.  To **query and merge data** from the `hudhears` database

2.  To **format or derive variables** necessary for analysis

3.  To **append Opportunity Index data** provided by PRSC (see [00_opportunity_index](#00_opportunity_index) for more information)

First, the script creates a connection to the `hhs_analytics_workspace`. To query, the database, being logged in to the KingCounty.gov VPN and having a valid KingCounty.gov username and password are necessary.

Next, three databases are queried. `pha.stage_pha_exit_timevar` is queried to obtain information on **exit times and exit types**. `hudhears.control_match_covariate` is queried to obtain **demographics and household covariates**. `hudhears.pha_homeless_status` is queried to obtain **homelessness status and start dates**.

The data are filtered to include the study population only: those who exited (specifically, only true exits and non-death exits) Seattle Housing Authority from 2012 to 2019 and those who exited King County Housing Authority from 2016 to 2019. The data are merged together on `id_hudhears`. Individuals who did not appear to have a homeless status are still merged to ensure those who have not experienced a homelessness event are included in the population.

*An intermediate table, `hudhears.capstone_data_1` is created after this step for debugging purposes.*

The code in the next section is confusing, but the general idea is that the merged data is then used to derive the outcome variable of interest, **earliest time to homelessness for the first year after exiting public housing**. To do this, a few rules are followed:

- `start_date`of homelessness must be after `exit_date`. There is a 30 day buffer implemented to account for up to a month of administrative lag in the input `exit_date` on file. Consequently, individuals who become homeless within 30 days prior to their exit date have a time to homelessness of 0 days.

- Only  a `housing_status` of "homeless" is considered a homelessness event. The `dummy_date` variable handles situations in which `housing_status` is not "homeless" by setting it to the `max_date`.

- For each individual, only the earliest occurance of homelessness after exiting public housing is counted using the `slice` function.

- The `tt_homeless` outcome variable is created by calculating the number of days between `dummy_date` and `exit_date`. A value greater than the `max_days` of 365 days are considered censored by the `event` indicator.

Two possible edits to the code may be necessary if methodology changes are made:

1. To change the 30 day administrative buffer, set `` to another length in days (TO DO: I need to confirm this part)

2. To change the 1 year censoring cutoff, set `max_date` to be `max_days` after the last `exit_date` in the data. (TO DO: Explain this better somehow...)

*An intermediate table, `hudhears.capstone_data_2` is created after this step for debugging purposes.*

The final formatting step is to read in `kc_opp_indices_scaled.csv`, to create geographic identifiers, and to merge the Opportunity Index data to the existing data by census tract.

*The final table, `hudhears.capstone_data_3` is created after this step for use in analysis.*

## 02_results

Once the `hudhears.capstone_data_3` table is successfully created, scripts in the `02_results` folder can be run to perform a variety of analyses on this formatted data. The scripts in the folder can be run in any order and are independent; the numbering is a suggestion and is representative of the order in which they were originally implemented.

### 01_descriptive_stats

**Usage:** This script is used to generate **descriptive statistics tables and visualizations of missingness patterns** from `hudhears.capstone_data_3`.

**Output:** Three descriptive statistics tables:

1. For censored individuals
2. For individuals who have homelessness events
3. For the overall population

Additionally producess a missingness frequency histogram and a missingness pattern plot.

**Details:**

The first part of the script implements the `table1` package to generate descriptive statistics typically found in Table 1. Each variable to be summarized is given an appropriate label, and categorical variables are renamed for readability. It is easy to edit the code to incorporate different variables or labels.

Three Table 1's are created based on whether the data are subset to those with events, subset to those who are censored, and not subset at all. It is straightforward to edit the code to only generate a Table 1 based on any subset of the data.

The last part of the script uses the `VIM` package to create a histogram of missing data frequency and a missingness pattern plot. It is possible to edit the code to select for a different subset of variables.

### 02_km_curve

**Usage:** This script is used to generate various **Kaplan-Meier curves comparing survival by exit type** from `hudhears.capstone_data_3`.

**Output:** Three Kaplan-Meier curves:

1. `KM_curve_plot.png` 
2. `KM_curve_pha_plot.png` stratified by SHA/KCHA
3. `KM_curve_pha_facet_plot.png` faceted by SHA/KCHA.

Additionally calculates K-M estimates for survival time at 365 days.

**Details:**

The script uses `ggsurvplot` to create Kaplan-Meier curves from survival objects produced from the `tt_homeless` and `event` variables of `hudhears.capstone_data_3`. The curves use complementary log-log confidence intervals.

To produce stratified K-M curves by SHA/KCHA, an additional variable indicating `agency` is included in the `survfit` prior to plotting. To produce plots faceted by SHA/KCA, an additional argument `facet.by="agency"` is implemented in the `ggsurvplot` function.

Additionally, `summary(fit_exit_type, times=365)` is used to extract the K-M estimator for survival time at 365 days for the three exit types.

### 03_regression

**Usage:** This script is used to perform **regression analysis exploring the association between `tt_homeless` and `exit_type`** from `hudhears.capstone_data_3`.

**Output:** Regression coefficients, standard errors, and p-values of the analysis. (TO DO: Add the results ggplot code to this script)

**Details:**

The first step is to calculate **propensity scores** of exit types using multinomial logistic regression through the `multgee` package. The covariates informing the propensity scores are defined in the `formula` argument of the `nomLORgee` function. Clustering by household ID is specified in  the `id` argument. A working independence covariance matrix is specified in the `LORstr` argument. It is simple to modify the logistic regression model as needed by adjusting the arguments.

Next, the script calculates the **inverse probability treatment weight** (IPTW) of each observation. This value is the inverse of the propensity score of each observed exit type.

Finally, a **weighted Cox proportional hazards model** is fit using the `coxph` function and by specifying the `weights` argument. Clustering by household ID is once again specified in the `cluster` argument.

Additionally, this script performs a sensitivity analysis using overlap weights instead of IPTW. To carry out the sensitivity analysis, the same steps from the primary analysis are used, where the only difference is in step 2. Rather than assigning weights equal to the inverse of the propensity score of each observed exit type, weights are assigned by subtracting the propensity score from 1. 

Moreover, this script repeats the primary analysis, but for subsets of the population. The first subset is for `agency = KCHA`, and the second subset is for `angecy = SHA`. The first two steps of the primary analysis (deriving propensity scores, and assigning weights via IPTW), are repeated. Then, before performing the third step (fitting the weighted Cox PH model), the data is subsetted by agency, and separate weighted Cox PH models are fit for each subsetted data set. 

(TO DO: Add ggplot code here.)

### 04_exit_reason_sensitivity

**Usage:** This script is used to perform a **sensitivity analysis exploring the impact of specific exit reasons** on the calculated hazard ratios, using data from `hudhears.capstone_data_3` and regression methods from [03_regression](#03_regression).

**Output:** Four forest plots illustrating the overall hazard ratios versus when each exit reason is removed:

1. `LOO_HR_pos_no_GEE.png`
2. `LOO_HR_pos_no_GEE_no_neg.png`
3. `LOO_HR_neg_no_GEE.png`
4. `LOO_HR_neg_no_GEE_no_pos.png`

**Details:** (TO DO: I need to talk to Taylor to see which parts of the code to keep and which to archive.)

The script begins by generating a list of all unique and non-null exit reasons.

Next, a function `run_analysis_no_GEE` is defined to run the primary analysis without GEE. The reason GEE is no longer used in the sensitivity analysis since omitting some exit reasons resulted in colinearity.

Another function `fit_one_out` is defined to omit all observations with a specified exit reason, run the analysis using `run_analysis_no_GEE`, and return results. These include the number of rows with the exit reason, the resulting hazard ratio, and confidence intervals for both positive vs. neutral and negative vs. neutral comparisons.

Then, `fit_one_out` is applied to each exit reason in the list of all exit reasons and  the results are stored. Before forest plots can be created, exit reasons with observations below a certain `count_threshold` are omitted to prevent the plots from becoming too large. The value is set to 100, but this value can be easily modified in the code.

Using the `forestplot` package, two forest plots are created: one displaying changes in hazard ratio upon removing various exit reasons when comparing negative and neutral exits, and one displaying changes in hazard ratio upon removing various exit reasons when comparing negative and neutral exits when comparing positive and neutral exits.

The `hrzl_lines` argument is a wide horizontal line to act as shading to differentiate different types of exits on one graph. If the graph is modified, it may be necessary to adjust the width of this horizontal line using the `lwd` argument. To modify the appearance of the forest plot in other ways, adjust any arguments in the `forestplot` function, referencing documentation if necessary. 
