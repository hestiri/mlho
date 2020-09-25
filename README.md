
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Hello MLHO\!

<!-- badges: start -->

<!-- badges: end -->

MLHO (pronounced as melo) is an end-to-end Machine Learning pipeline
that implements iterative sequential representation mining, and feature
and model selection to predict health outcomes.

## Installation

You can install the released version of mlho from
[Github](https://https://github.com/hestiri/mlho) with:

``` r
devtools::install_github("hestiri/mlho")
```

## Data model

To implement MLHO you’ll need 2 tables, which can be extracted from any
clinical CMD. The current examples are based on the i2b2 star schema.

1- a table with outcome labels (called labeldt) and patient numbers

|              |        |
| :----------- | :----- |
| patient\_num | label  |
| character    | factor |

2- a patient clinical data table (called dbmart) with 3 columns.
Concepts are used as features by MLHO.

|              |             |            |
| :----------- | :---------- | :--------- |
| patient\_num | start\_date | oncept\_cd |
| character    | date        | character  |

3- a demographic table is optional.
