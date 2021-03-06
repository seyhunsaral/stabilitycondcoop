![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)
![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)
[![DOI](https://zenodo.org/badge/202017365.svg)](https://zenodo.org/badge/latestdoi/202017365)

# The Stability of Conditional Cooperation (Andreozzi, Ploner, Saral, 2020) 
## About this repository 
This repository contains the supplementary files for the study:

[Andreozzi, Luciano, Matteo Ploner, and Ali Seyhun Saral. "The Stability of Conditional Cooperation: Beliefs Alone Cannot Explain the Decline of Cooperation in Social Dilemmas" Scientific Reports 10, 13610 (2020).](https://www.nature.com/articles/s41598-020-70681-z)

The reposiotory contains z-Tree files to run the experiment, the data, and the R codes for the data analysis. The figures and the tables can be reproduced using the scripts in the repository. The repository also contains a `zBrac` compatible treatment file and the language template for easy translation/replication of the experiment. (For more information about the zBrac project [click here for the project repository](https://www.github.com/seyhunsaral/zbrac))

The data (`/data`) are licensed under Creative Commons Attribution 4.0 international license. The files contain z-Tree (`/ztree`) and R codes(`/code`) are licensed under MIT license. See the [LICENSE](LICENSE) file for the license details.

[Click here to download this repository](https://github.com/seyhunsaral/stabilitycondcoop/archive/master.zip)

#### Index of README

* [Repository Contents](#repository-contents)
    * [code](#code)
    * [data](#data)
      * [csv](#csv)
      * [rawdata](#rawdata)
      * [rds](#rds)
    * [figs](#figs)
    * [ztree](#ztree)
      * [original-italian](#original-italian)
* [Reproduction](#reproduction)
   * [Session information and dependencies](#my-session-information-and-dependencies-sessioninfo)
* [List of variables in the data](#list-of-variables-in-the-data)
* [List of variables in z-Tree Files](#list-of-variables-in-z-tree-files)

## Repository Contents
### code
 * `functions.R` : Contains the functions that are used for the analysi 
 * `main.R` : This file reproduces the figures and the tables in the paper.
 * `process_raw_data.R`: This file takes the raw data in `./data/rawdata/` folder and produces CSV and RDS files for the analysis. The repository already contains the cleaned-up data files (CSV and RDS). However these files are available only for the sake of reproduction.
 
### data
 
#### csv
 Generated by `process_raw_data.R`. 
 * `decbehav.csv`: Subjects' interactions with the interface is recorded here. Not used for the analysis
 * `quest.csv`: Post-experiment questionnaire data
 * `subjects.csv`: Main data file
 
#### rawdata
 This folder contains raw data of each session as the output generated by z-Tree.
 
#### rds
 Generated by `process_raw_data.R`. R Data Tables in R Object form. Used for the analysis. The advantage of those files over the CSV files is that they contain the data types of each column as well. For the definitions see `csv` section above.
 * `decbehav.csv`
 * `quest.csv`
 * `subjects.csv`
 
### figs
 Empty folder for the figures to be generated by `main.R`
 
### ztree
 * `condcoop_english.txt`: English version of the main experiment. Created with. `zBrac`. Should be imported by z-Tree.
 * `cond_coop_zbrac_language_template.xlsx` Language file for the zBrac. It can be edited and used with `condcoop_zbrac.txt` to generate the experiment in other languages.
 * `condcoop_zbrac.txt`: `zBrac` The source file. It can be used to generate treatment files in different languages.
 #### original-italian
 Contains the original z-Tree files in Italian language
 * `1_control_questions.ztt` : Control questions
 * `2_condcoop.ztt`: Experiment
 * `3_quest.ztq`: Questionnaire

## Reproduction 
* To reproduce the figures and tables, the script `main.R` should be executed. That would print out the tables on the console and create figures in the folder `figs`.

* To reproduce the data cleaning process, the script `process_raw_data.R` should be run. That would recreate the data in `csv` and `rds` folders.

On major operating systems, it can be done through the command line as the following (given that the R software and the dependencies are installed).
```
Rscript process_raw_data.R
Rscript main.R 
```

### Session information and the dependencies: `sessionInfo()`

```
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] zTree_1.0.6      ggalluvial_0.9.1 optimx_2018-7.10 nnet_7.3-12     
 [5] xtable_1.8-4     texreg_1.36.23   lmerTest_3.1-0   lme4_1.1-21     
 [9] Matrix_1.2-17    gmodels_2.18.1   purrr_0.3.2      forcats_0.4.0   
[13] ggrepel_0.8.1    vipor_0.4.5      stringr_1.4.0    tidyr_0.8.3     
[17] ggplot2_3.2.0    dplyr_0.8.1      here_0.1
```

## List of variables in the data 

| Variable                   | Table    | Description                                                                 |
|----------------------------|----------|-----------------------------------------------------------------------------|
| treatment                  | subjects | Treatment: `NoCondInfo` or `CondInfo`                                       |
| session                    | subjects | session number                                                              |
| period                     | subjects | period number                                                               |
| subject                    | subjects | subject number                                                              |
| group                      | subjects | group number                                                                |
| first_player               | subjects | logical - if the player is selected as the first player                     |
| uc                         | subjects | choice as the first player                                                  |
| ccL                        | subjects | conditional choice if the other player chooses L (0)                        |
| ccM                        | subjects | conditional choice if the other player chooses M  (50)                      |
| ccH                        | subjects | conditional choice if the other player chooses H  (100)                     |
| action                     | subjects | action that was actualized (after the random role assingment)               |
| opp                        | subjects | opponents number                                                            |
| ouc                        | subjects | opponent's choice of the opponent                                           |
| occL                       | subjects | opponent's conditional choice for L (0)                                     |
| occM                       | subjects | opponent's conditional choice for M (50)                                    |
| occH                       | subjects | opponent's conditional choice for H (100)                                   |
| buc_L                      | subjects | beliefs - probability opponent plays L in unconditonal choice               |
| buc_M                      | subjects | beliefs - probability opponent plays M in unconditonal choice               |
| buc_H                      | subjects | beliefs - probability opponent plays H in unconditonal choice               |
| bccL_L                     | subjects | beliefs - probability opponent plays L in response to L                     |
| bccL_M                     | subjects | beliefs - probability opponent plays M in response to L                     |
| bccL_H                     | subjects | beliefs - probability opponent plays H in response to L                     |
| bccM_L                     | subjects | beliefs - probability opponent plays L in response to M                     |
| bccM_M                     | subjects | beliefs - probability opponent plays M in response to M                     |
| bccM_H                     | subjects | beliefs - probability opponent plays H in response to M                     |
| bccH_L                     | subjects | beliefs - probability opponent plays L in response to H                     |
| bccH_M                     | subjects | beliefs - probability opponent plays M in response to H                     |
| bccH_H                     | subjects | beliefs - probability opponent plays H in response to H                     |
| opp_action                 | subjects | opponent's action that was actualized (after the random role assignment)    |
| earning_kept               | subjects | Earnings from the amount kept                                               |
| earning_received           | subjects | Earnings from the opponent's transfer                                       |
| earning_guess              | subjects | Earninns from the expectations period (beliefs)                             |
| earning_game               | subjects | Earnings from the decision phase (EarningKept + EarningReceived)            |
| ucf                        | subjects | Factorized letter form of uc                                                |
| ccLf                       | subjects | Factorized letter form of ccL                                               |
| ccMf                       | subjects | Factorized letter form of ccM                                               |
| ccHf                       | subjects | Factorized letter form of ccH                                               |
| oucf                       | subjects | Factorized letter form of ouc                                               |
| occLf                      | subjects | Factorized letter form of occL                                              |
| occMf                      | subjects | Factorized letter form of occM                                              |
| occHf                      | subjects | Factorized letter form of occH                                              |
| subject_unq                | subjects | Unique subject number                                                       |
| session_unq                | subjects | Unique session number                                                       |
| opp_unq                    | subjects | Unique subject number of the opponent                                       |
| cond_type                  | subjects | Conditional type number (base 10 conversion of base 3 letters)              |
| opp_cond_type              | subjects | Conditional type number of the opponent                                     |
| cond_class_wide            | subjects | Conditional type classification (wide classification)                       |
| cond_class_wide_o          | subjects | Conditional type classification (wide classification with `other`)          |
| cond_class_narrow          | subjects | Conditional type classification (narrow classification)                     |
| opp_cond_class_wide        | subjects | Opponent's conditional type classification (wide classification)            |
| opp_cond_class_wide_o      | subjects | Opponent's conditional type classifcation (wide classification with `other` |
| opp_cond_class_narrow      | subjects | Opponent's conditional type classification (narrow classifcation)           |
| exp_pay_uc                 | subjects | Expected payoff from the opponents move as the first player                 |
| exp_pay_ccL                | subjects | Expected payoff from the opponents respone to L                             |
| exp_pay_ccM                | subjects | Expected payoff from the opponents respone to M                             |
| exp_pay_ccH                | subjects | Expected payoff from the opponents respone to H                             |
| opp_action_lag1            | subjects | Action of the previous opponent in the previous period (lagged variable)    |
| opp_cond_type_lag1         | subjects | Type of the previous opponent in the previous period (lagged variable)      |
| opp_cond_class_wide_lag1   | subjects | see `opp_cond_class_wide` (lagged variable)                                 |
| opp_cond_class_wide_o_lag1 | subjects | see `opp_cond_class_wide_o` (lagged variable)                               |
| opp_cond_class_narrow_lag1 | subjects | see `opp_cond_class_narrow` (lagged variable)                               |
| first_player_lag1          | subjects | logical - if first player in the previous period (lagged variable)          |
| isCC                       | subjects | logical - if conditional cooperator                                         |
| isS                        | subjects | logical - if selfish                                                        |
| isHS                       | subjects | logical - if hump-shaped                                                    |


## List of variables in z-Tree files
| Variable          | Table    | Type      | Description                                                              |
|-------------------|----------|-----------|--------------------------------------------------------------------------|
| IE                | globals  | constant  | Initial Endowment                                                        |
| IEg               | globals  | constant  | Initial Endowment for the beliefs                                        |
| cL                | globals  | constant  | Low contribution amount                                                  |
| cM                | globals  | constant  | Medium contribution amount                                               |
| cH                | globals  | constant  | High contribution amount                                                 |
| PeriodSelected    | globals  | constant  | Selected period for payments                                             |
| RegularTimeOut    | globals  | constant  | Timeout for stages                                                       |
| Debug             | globals  | debugging | Debug mode on (1) off (0)                                                |
| DebugTimeOut      | globals  | debugging | Timeout when debug mode is on                                            |
| dbgFocusOnStage   | globals  | debugging | Stage to go directly when debug mode is on                               |
| dbgShouldLeave    | globals  | debugging | Condition to leave stage when debug mode is on                           |
| dbgCurrentStage   | globals  | debugging | Stage number to be used for skipping to a certain stage                  |
| FirstPlayer       | subjects | game      | 1 if the player is selected to be the first player, 0 otherwise          |
| uc                | subjects | game      | choice as the first player                                               |
| ccL               | subjects | game      | conditional choice if the other player chooses L (0)                     |
| ccM               | subjects | game      | conditional choice if the other player chooses M  (50)                   |
| ccH               | subjects | game      | conditional choice if the other player chooses H  (100)                  |
| Action            | subjects | game      | action that was actualized (after the random role assingment)            |
| opp               | subjects | game      | opponents number                                                         |
| ouc               | subjects | game      | opponent's choice of the opponent                                        |
| occL              | subjects | game      | opponent's conditional choice for L (0)                                  |
| occM              | subjects | game      | opponent's conditional choice for M (50)                                 |
| occH              | subjects | game      | opponent's conditional choice for H (100)                                |
| buc_L             | subjects | game      | beliefs - probability opponent plays L in unconditonal choice            |
| buc_M             | subjects | game      | beliefs - probability opponent plays M in unconditonal choice            |
| buc_H             | subjects | game      | beliefs - probability opponent plays H in unconditonal choice            |
| bccL_L            | subjects | game      | beliefs - probability opponent plays L in response to L                  |
| bccL_M            | subjects | game      | beliefs - probability opponent plays M in response to L                  |
| bccL_H            | subjects | game      | beliefs - probability opponent plays H in response to L                  |
| bccM_L            | subjects | game      | beliefs - probability opponent plays L in response to M                  |
| bccM_M            | subjects | game      | beliefs - probability opponent plays M in response to M                  |
| bccM_H            | subjects | game      | beliefs - probability opponent plays H in response to M                  |
| bccH_L            | subjects | game      | beliefs - probability opponent plays L in response to H                  |
| bccH_M            | subjects | game      | beliefs - probability opponent plays M in response to H                  |
| bccH_H            | subjects | game      | beliefs - probability opponent plays H in response to H                  |
| oAction           | subjects | game      | opponent's action that was actualized (after the random role assignment) |
| EarningKept       | subjects | game      | Earnings from the amount kept                                            |
| EarningReceived   | subjects | game      | Earnings from the opponent's transfer                                    |
| EarningGuess      | subjects | game      | Earninns from the expectations period (beliefs)                          |
| EarningGame       | subjects | game      | Earnings from the decision phase (EarningKept + EarningReceived)         |
| FinalEarningGame  | subjects | game      | Earnings from the decision phase in the selected period                  |
| FinalEarningGuess | subjects | game      | Earnings from the expectations phase in the selected period              |


