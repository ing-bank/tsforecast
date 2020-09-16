
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/tsforecast_logo.png" width="150px" />

[![lifecycle](man/figures/lifecycle-stable-brightgreen.png)](https://www.tidyverse.org/lifecycle/#stable)

  - [Overview](#overview)
  - [Features](#features)
  - [Installation](#installation)
  - [How to use](#how_to_use)
  - [Contribute](#contribute)
  - [Acknowledgments](#acknowledgments)

## Overview<a name="overview"></a>

tsforecast contains a set of R functions that can be used to do time
series forecasting on monthly data, for the following cases:

  - Univariate forecasting:
      - Time series forecasting without external regressors (so
        regressing on the observations themselves)
  - Multivariate forecasting:
      - Time series forecasting including external regressors (a.k.a.
        predictor/feature variables or covariates)

-----

The package attempts to streamline the process of doing time series
forecasting in R by abstracting away some of the technicalities, such
as:

  - Creating time series objects from your data
  - Splitting up the time series objects into train and test sets using
    a sliding window
  - Comparing the performance of multiple algorithms at one go, such as:
      - Simple heuristics (e.g. mean and drift models)
      - ‘Classic’ econometric methods (e.g. ARIMA and Holt-Winters)
      - Machine Learning algorithms (e.g. neural networks and random
        forests)
      - … and more (e.g. Prophet)
  - Allow for easy analysis of the results in an interactive dashboard

-----

For more information on definitions, available forecast models and their
implementation in the tsforecast package, check out the documentation by
either:

  - Running the `show_documentation()` function after
    [installing](#installation) and loading the package
  - Manually downloading [this
    file](inst/documentation/forecast_models.html) from Github and
    opening it in your browser (until Github makes it possible to view
    rendered html …)

The original developers of this R package are [Gertjan van den
Bos](mailto:gertjan.bos@ing.com), [Mehmet
Kutluay](mailto:yasar.kutluay@ing.com), [Olle
Dahlen](mailto:olle.dahlen@ing.com), [Miel
Verkerken](mailto:mielverkerken@hotmail.com), [Han
Lin](mailto:han.lin@ing.com) & [Berke
Aslan](mailto:berke.aslan@edu.devinci.fr)

## Features<a name="features"></a>

The following functions are available within the time series forecasting
framework in this R package.

  - Create forecasts:
      - `update_main_forecasting_table()`<a name="update_main_forecasting_table"></a>:
        is a wrapper function around the following functions, to EITHER
        initialize a set of new forecasts (and write these to disk for
        reuse later on) OR update an existing set of forecasts (that was
        previously stored on disk to prevent recalculation) with new
        forecasts that can be created as new data becomes available:
          - `create_main_forecasting_table()`<a name="create_main_forecasting_table"></a>:
            is a function to create a table in which every row
            represents a different split of the data for time series
            forecasting.
          - `add_fc_models_to_main_forecasting_table()`<a name="add_fc_models_to_main_forecasting_table"></a>:
            is a function to extend the main forecasting table with an
            additional column, called fc\_models, in which all the
            specified forecast models per method are stored after they
            have been run (check the documentation for the available
            forecast models). The function will also extend the main
            forecasting table with another additional column, called
            fc\_errors, which contains the forecast values, actuals and
            resulting forecast errors for every forecast model in the
            fc\_models column.
      - `add_mean_ensemble_fc_methods()`<a name="add_mean_ensemble_fc_methods"></a>:
        is a function to create an ensemble forecast model, based on
        taking the mean of the forecasted values for a specfied
        selection of forecast models that are available in the main
        forecasting table. This approach is based on the knowledge that
        combining forecasts often leads to better forecast accuracy.
  - Evaluate forecasts:
      - `get_forecast_accuracy_overview()`<a name="get_forecast_accuracy_overview"></a>:
        is a function to create an overview of the accuracy of the
        different forecast models in the main forecasting table.
      - `get_best_forecast_methods()`<a name="get_best_forecast_methods"></a>:
        is a function to determine the n best forecast methods from an
        (ordered) accuracy overview, when evaluating for a specific
        minimum and maximum number of periods ahead forecast horizon.
      - `start_forecast_analysis_dashboard()`<a name="start_forecast_analysis_dashboard"></a>:
        is a function to start an interactive Shiny dashboard that
        enables exploration of the individual forecasts versus actuals
        per split date, as well as an analysis of the overall forecast
        accuracy of the different forecast models.

-----

If you want to learn more about which forecast models are available in
the package, then please check out the documentation (after
[installing](#installation) the package):

``` r
# Open the documentation on the available forecast models in your browser
tsforecast::show_documentation(topic = "forecast_models")
```

-----

If you want to see a quick demo of the available forecast methods on a
public dataset, please run one or more of the examples (after
[installing](#installation) the package):

``` r
library(tsforecast)

# AirPassengers: The classic Box & Jenkins airline data. Monthly totals of international airline passengers, 1949 to 1960
run_example(dataset = "AirPassengers")

# nottem: A time series object containing average air temperatures at Nottingham Castle in degrees Fahrenheit for 20 years
run_example(dataset = "nottem")

# UKDriverDeaths: A time series giving the monthly totals of car drivers in Great Britain killed or seriously injured Jan 1969 to Dec 1984
run_example(dataset = "UKDriverDeaths")
```

These examples make use of the same Shiny dashboard as when using the
`start_forecast_analysis_dashboard()` function:

![](man/figures/tsforecast_run_example_printscreen.png)

## Installation<a name="installation"></a>

tsforecast is a package developed within ING and is not available on
CRAN, but on [INGs github](https://github.com/ing-bank). You can install
the package directly from github using the [devtools
package](https://cran.r-project.org/web/packages/devtools/index.html),
using:

``` r
devtools::install_github("ing-bank/tsforecast")
```

Some prerequisites for installing the package:

  - R version 3.6.0 or later
  - Rtools installed ([How
    to?](https://thecoatlessprofessor.com/programming/installing-rtools-for-compiled-code-via-rcpp))

Any required packages that are missing from your R library should be
automatically installed for you, otherwise please [install any missing
packages](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/install.packages.html)
before using the tsforecast package.

**PLEASE NOTE**: *after installing the prophet package, you might need
to call `library(prophet)` once in R for it to compile its model before
you can use it\!*

## How to use<a name="how_to_use"></a>

The package contains example data (a modified version of
expsmooth::gasprice) that can be used to try out the package:

``` r
library(tsforecast)
#> Loading required package: Rcpp
#> Warning: package 'Rcpp' was built under R version 3.6.3
#> Loading required package: caret
#> Warning: package 'caret' was built under R version 3.6.3
#> Loading required package: lattice
#> Loading required package: ggplot2
#> Loading required package: tstools
#> Loading required package: magrittr
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Registered S3 method overwritten by 'greybox':
#>   method     from
#>   print.pcor lava
head(dummy_gasprice)
#> # A tibble: 6 x 6
#>   year_month state    oil_company gasprice spotprice gemprice
#>   <date>     <chr>    <chr>          <dbl>     <dbl>    <dbl>
#> 1 1991-01-31 New York CompanyA        1.22      29.4     44.5
#> 2 1991-02-28 New York CompanyA        1.22      18.5     41.8
#> 3 1991-03-31 New York CompanyA        1.16      16.5     38.8
#> 4 1991-04-30 New York CompanyA        1.26      25.0     59.0
#> 5 1991-05-31 New York CompanyA        1.33      24.6     44.5
#> # ... with 1 more row
```

You need to initialize the data to be used within the time series
forecasting framework (using
[`initialize_ts_forecast_data()`](#initialize_ts_forecast_data) from the
[tstools](https://github.com/ing-bank/tstools) package), by specifying
which columns correspond to which required fields for the forecasting:

``` r
# For UNIVARIATE forecasting
ts_forecast_data_univariate <- tstools::initialize_ts_forecast_data(
  data = dummy_gasprice, 
  # Indicate which Date column corresponds to the time component
  date_col = "year_month", 
  # Indicate which column of values should be forecasted
  col_of_interest = "gasprice",
  # OPTIONAL: Indicate which column(s) should be used to create groups to forecast separately
  group_cols = c("state", "oil_company")
)
head(ts_forecast_data_univariate)
#> # A tibble: 6 x 3
#>   period     col_of_interest grouping                                     
#>   <date>               <dbl> <chr>                                        
#> 1 1991-01-31            1.22 state = New York   &   oil_company = CompanyA
#> 2 1991-02-28            1.22 state = New York   &   oil_company = CompanyA
#> 3 1991-03-31            1.16 state = New York   &   oil_company = CompanyA
#> 4 1991-04-30            1.26 state = New York   &   oil_company = CompanyA
#> 5 1991-05-31            1.33 state = New York   &   oil_company = CompanyA
#> # ... with 1 more row

# For MULTIVARIATE forecasting
ts_forecast_data_multivariate <- tstools::initialize_ts_forecast_data(
  data = dummy_gasprice, 
  date_col = "year_month", 
  col_of_interest = "gasprice", 
  group_cols = c("state", "oil_company"),
  # For multivariate forecasting, indicate which column(s) should be used as external regressors
  xreg_cols = c("spotprice", "gemprice")
)
head(ts_forecast_data_multivariate)
#> # A tibble: 6 x 5
#>   period     col_of_interest grouping                                     
#>   <date>               <dbl> <chr>                                        
#> 1 1991-01-31            1.22 state = New York   &   oil_company = CompanyA
#> 2 1991-02-28            1.22 state = New York   &   oil_company = CompanyA
#> 3 1991-03-31            1.16 state = New York   &   oil_company = CompanyA
#> 4 1991-04-30            1.26 state = New York   &   oil_company = CompanyA
#> 5 1991-05-31            1.33 state = New York   &   oil_company = CompanyA
#>   spotprice gemprice
#>       <dbl>    <dbl>
#> 1      29.4     44.5
#> 2      18.5     41.8
#> 3      16.5     38.8
#> 4      25.0     59.0
#> 5      24.6     44.5
#> # ... with 1 more row
```

-----

Using the wrapper function
[`update_main_forecasting_table()`](#update_main_forecasting_table),
which stores the main forecasting table on disk (to prevent lengthy
recalculations and enable later expansion with new data and/or forecast
methods):

``` r
main_forecasting_table <- update_main_forecasting_table(
  # Change the path to write to a different location (instead of the current working directory)
  file_path = file.path(tempdir(), "example_forecast_file.rds"), 
  # Using the multivariate forecast data for this example, but univariate is also possible 
  data = ts_forecast_data_multivariate, 
  # Default is quarterly (every 3 months) and yearly (every 12 months) seasonality
  seasonal_periods = c(12,3), 
  # Default is 24 months (2 years) of miminum training period, but 180 month is used here to limit the runtime
  min_train_periods = 180, 
  # By default the training data is not limited
  max_train_periods = Inf, 
  # 12 months ahead forecasting is the default forecast horizon
  periods_ahead = 12, 
  # Limit the forecast methods to only two methods to limit the runtime
  fc_methods = c("linear", "prophet"),
  # Set to TRUE to get updates on progress
  verbose = FALSE
)
head(main_forecasting_table)
#> # A tibble: 6 x 10
#>   grouping                                      ts_start ts_split_date ts_end
#>   <chr>                                            <dbl>         <dbl>  <dbl>
#> 1 state = New York   &   oil_company = CompanyA   199101        200512 200611
#> 2 state = New York   &   oil_company = CompanyA   199101        200601 200611
#> 3 state = New York   &   oil_company = CompanyA   199101        200602 200611
#> 4 state = New York   &   oil_company = CompanyA   199101        200603 200611
#> 5 state = New York   &   oil_company = CompanyA   199101        200604 200611
#>   train_length valid_length ts_object_train ts_object_valid fc_models       
#>          <dbl>        <dbl> <list>          <list>          <list>          
#> 1          180           11 <msts>          <msts>          <named list [5]>
#> 2          181           10 <msts>          <msts>          <named list [5]>
#> 3          182            9 <msts>          <msts>          <named list [5]>
#> 4          183            8 <msts>          <msts>          <named list [5]>
#> 5          184            7 <msts>          <msts>          <named list [5]>
#>   fc_errors        
#>   <list>           
#> 1 <tibble [55 x 9]>
#> 2 <tibble [50 x 9]>
#> 3 <tibble [45 x 9]>
#> 4 <tibble [40 x 9]>
#> 5 <tibble [35 x 9]>
#> # ... with 1 more row
```

Alternatively, instead of using the wrapper function you can combine the
underlying functions to achieve the same results (see
[features](#features) for more information), as demonstrated below.
However, you need to manually (re)store the main\_forecasting\_table
to/from disk (using writeRDS() and readRDS()) if required, while the
wrapper function handles this for you.

-----

After initializing the forecast data, the main forecasting table can be
created while specifying the requirements for the time series forecasts
using the
[`create_main_forecasting_table()`](#create_main_forecasting_table)
function:

``` r
main_forecasting_table <- create_main_forecasting_table(
  # Using the univariate forecast data for this example, but multivariate is also possible 
  data = ts_forecast_data_univariate, 
  # Default is quarterly (every 3 months) and yearly (every 12 months) seasonality
  seasonal_periods = c(12,3),
  # Miminum training period of 60 months (5 years) is used here
  min_train_periods = 5 * 12,
  # By default the training data is not limited
  max_train_periods = Inf
)
head(main_forecasting_table)
#> # A tibble: 6 x 8
#>   grouping                                      ts_start ts_split_date ts_end
#>   <chr>                                            <dbl>         <dbl>  <dbl>
#> 1 state = New York   &   oil_company = CompanyA   199101        199512 200611
#> 2 state = New York   &   oil_company = CompanyA   199101        199601 200611
#> 3 state = New York   &   oil_company = CompanyA   199101        199602 200611
#> 4 state = New York   &   oil_company = CompanyA   199101        199603 200611
#> 5 state = New York   &   oil_company = CompanyA   199101        199604 200611
#>   train_length valid_length ts_object_train ts_object_valid
#>          <dbl>        <dbl> <list>          <list>         
#> 1           60          131 <msts>          <msts>         
#> 2           61          130 <msts>          <msts>         
#> 3           62          129 <msts>          <msts>         
#> 4           63          128 <msts>          <msts>         
#> 5           64          127 <msts>          <msts>         
#> # ... with 1 more row
```

Initially, the main forecasting table only contains the settings and
requirements for performing the time series forecasting. The forecast
models, forecasts and resulting forecast errors are added using the
[`add_fc_models_to_main_forecasting_table()`](#add_fc_models_to_main_forecasting_table)
function:

``` r
main_forecasting_table <- add_fc_models_to_main_forecasting_table(
  main_forecasting_table = main_forecasting_table, 
  # 12 months ahead forecasting is the default forecast horizon
  periods_ahead = 12,
  # Limit the forecast methods to only two methods to limit the runtime
  fc_methods = c("linear", "holt_winters"),
  # Set to TRUE to get updates on progress
  verbose = FALSE
)
head(main_forecasting_table)
#> # A tibble: 6 x 10
#>   grouping                                      ts_start ts_split_date ts_end
#>   <chr>                                            <dbl>         <dbl>  <dbl>
#> 1 state = New York   &   oil_company = CompanyA   199101        199512 200611
#> 2 state = New York   &   oil_company = CompanyA   199101        199601 200611
#> 3 state = New York   &   oil_company = CompanyA   199101        199602 200611
#> 4 state = New York   &   oil_company = CompanyA   199101        199603 200611
#> 5 state = New York   &   oil_company = CompanyA   199101        199604 200611
#>   train_length valid_length ts_object_train ts_object_valid fc_models       
#>          <dbl>        <dbl> <list>          <list>          <list>          
#> 1           60          131 <msts>          <msts>          <named list [4]>
#> 2           61          130 <msts>          <msts>          <named list [4]>
#> 3           62          129 <msts>          <msts>          <named list [4]>
#> 4           63          128 <msts>          <msts>          <named list [4]>
#> 5           64          127 <msts>          <msts>          <named list [4]>
#>   fc_errors        
#>   <list>           
#> 1 <tibble [48 x 9]>
#> 2 <tibble [48 x 9]>
#> 3 <tibble [48 x 9]>
#> 4 <tibble [48 x 9]>
#> 5 <tibble [48 x 9]>
#> # ... with 1 more row
```

-----

With all the forecast models, forecasts and forecast errors available in
the main forecasting table, it is now possible to calculate the forecast
performance in terms of forecasting accuracy using the
[`get_forecast_accuracy_overview()`](#get_forecast_accuracy_overview)
function:

``` r
accuracy_overview <- get_forecast_accuracy_overview(
  main_forecasting_table = main_forecasting_table,
  # By default, the Mean Absolute Error (MAE) is used to evaluate the forecast errors and rank the models
  metric = "MAE"
)
head(accuracy_overview)
#> # A tibble: 6 x 14
#>   grouping                                     fc_periods_ahead
#>   <chr>                                                   <dbl>
#> 1 state = Indiana   &   oil_company = CompanyA                1
#> 2 state = Indiana   &   oil_company = CompanyA                1
#> 3 state = Indiana   &   oil_company = CompanyA                1
#> 4 state = Indiana   &   oil_company = CompanyA                1
#> 5 state = Indiana   &   oil_company = CompanyA                2
#>   fc_model                 n_data_point   MAE  MAPE  MASE      min     q1 metric
#>   <chr>                           <int> <dbl> <dbl> <dbl>    <dbl>  <dbl>  <dbl>
#> 1 fc_holt_winters_addiv             131 0.200 0.134  1.04 0.000197 0.0727  0.200
#> 2 fc_holt_winters_multip            131 0.203 0.136  1.05 0.00169  0.0761  0.203
#> 3 fc_linear_trend                   131 0.274 0.163  1.36 0.00577  0.104   0.274
#> 4 fc_linear_trend_seasonal          131 0.279 0.166  1.39 0.000898 0.0985  0.279
#> 5 fc_holt_winters_addiv             130 0.205 0.137  1.07 0.00181  0.0664  0.205
#>      q3   max    sd order
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.266  1.20 0.177     1
#> 2 0.277  1.21 0.176     2
#> 3 0.352  1.55 0.265     3
#> 4 0.392  1.53 0.265     4
#> 5 0.286  1.20 0.184     1
#> # ... with 1 more row
```

Finally, we can use the overview of the forecast accuracy to determine
(for each group) which are the top n best performing forecast methods
according to our evaluation criteria (in this case Mean Absolute Error
(MAE)) using the
[`get_best_forecast_methods()`](#get_best_forecast_methods) function:

``` r
get_best_forecast_methods(
  accuracy_overview = accuracy_overview,
  # Specify the top n forecast models to show, in this case only the best model per grouping is returned
  n = 1,
  # The minimum/maximum forecast horizon to consider when evaluating the forecast models, in this case all available forecast horizons are considered
  min_periods_ahead = 1,
  max_periods_ahead = Inf
)
#> # A tibble: 4 x 3
#>   grouping                                      fc_model              ranking
#>   <chr>                                         <chr>                   <dbl>
#> 1 state = Indiana   &   oil_company = CompanyA  fc_holt_winters_addiv       1
#> 2 state = Indiana   &   oil_company = CompanyB  fc_linear_trend             1
#> 3 state = New York   &   oil_company = CompanyA fc_holt_winters_addiv       1
#> 4 state = New York   &   oil_company = CompanyB fc_holt_winters_addiv       1
```

For a more detailed analysis of the results, we can start an interactive
Shiny dashboard which enables exploration of the individual forecasts
versus actuals per split date, as well as an analysis of the overall
forecast accuracy of the different forecast models, using the
[`start_forecast_analysis_dashboard()`](#start_forecast_analysis_dashboard)
function:

``` r
start_forecast_analysis_dashboard(
  main_forecasting_table = main_forecasting_table
)
```

## Contribute<a name="contribute"></a>

  - Idea? Please open an issue
  - Bug? Please open an issue
  - Want to contribute? Awesome\! Please open an issue :)

## License<a name ="license"></a>

This package is free and open source software, licensed under GPL-3.
More information can be found
[here](https://www.gnu.org/licenses/gpl-3.0.en.html).

## Acknowledgments<a name="acknowledgments"></a>

This package relies heavily on a number of other packages which
implement different kind of time series forecasting methods:

| CRAN                                                                               | Github                                                 | Background information                                                                                                                               |
| ---------------------------------------------------------------------------------- | ------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| [forecast](https://cran.r-project.org/web/packages/forecast/index.html)            | [robjhyndman](https://github.com/robjhyndman/forecast) | [Forecasting: principles and practice](https://www.otexts.org/book/fpp)                                                                              |
| [prophet](https://cran.r-project.org/web/packages/prophet/index.html)              | [facebook](https://github.com/facebook/prophet)        | [Forecasting at scale](https://facebook.github.io/prophet/)                                                                                          |
| [nnfor](https://cran.r-project.org/web/packages/nnfor/index.html)                  | [trnnick](https://github.com/trnnick/nnfor)            | [Forecasting time series with neural networks in R](http://kourentzes.com/forecasting/2017/02/10/forecasting-time-series-with-neural-networks-in-r/) |
| [rpart](https://cran.r-project.org/web/packages/rpart/index.html)                  | [cran](https://github.com/cran/rpart)                  | [An Introduction to Recursive Partitioning Using the RPART Routines](https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf)          |
| [party](https://cran.r-project.org/web/packages/party/index.html)                  | [rforge](https://github.com/rforge/party)              | [A Laboratory for Recursive Partytioning](http://party.r-forge.r-project.org/)                                                                       |
| [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html)    | [cran](https://github.com/cran/randomForest)           | [Ensemble-of-trees-for-forecasting-time-series](https://petolau.github.io/Ensemble-of-trees-for-forecasting-time-series/)                            |
| [forecastHybid](https://cran.r-project.org/web/packages/forecastHybrid/index.html) | [ellisp](https://github.com/ellisp/forecastHybrid)     | [Forecast Combinations](https://robjhyndman.com/hyndsight/forecast-combinations/)                                                                    |
| [caret](https://cran.r-project.org/web/packages/caret/index.html)                  | [topepo](https://github.com/topepo/caret)              | [\_C\_lassification \_A\_nd \_RE\_gression \_T\_raining](http://topepo.github.io/caret/index.html)                                                   |
| [dlm](https://cran.r-project.org/web/packages/dlm/index.html)                      | [cran](https://github.com/cran/dlm)                    | [State space models in *dlm*](https://robjhyndman.com/talks/ABS3.pdf#page=32&zoom=100,0,-1)                                                          |
