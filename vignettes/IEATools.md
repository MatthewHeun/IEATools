---
title: "IEATools"
author: "Matthew Kuperus Heun"
date: "2020-01-15"
output: 
  rmarkdown::html_vignette:
    fig_caption: yes
vignette: >
  %\VignetteIndexEntry{Introduction To IEATools}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Introduction

`IEATools` is an `R` package that provides functions to analyze 
extended energy balance data from the 
[International Energy Agency (IEA)](https://www.iea.org).
The IEA's data are not free. 
But if you have access to the data, you can follow the instructions in this vignette to 
analyze it.


## Getting started

To get started, data must be obtained and pulled into an R data frame.

### Obtain IEA data

Purchase the IEA extended energy balances product from (http://data.iea.org).
Download the complete extended energy balance data for at least one country
in a .csv file format as shown in the following figure.

<embed src="figs/original_header.pdf" title="IEA extended energy balance data format." alt="IEA extended energy balance data format." width="100%" type="application/pdf" />

Example data from two countries [Ghana (GH) and South Africa (ZA)]
for two years (1971 and 2000) are provided in the `IEATools` package.


```r
library(dplyr)
library(IEATools)
# Define the file location
IEA_path <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
  system.file(package = "IEATools")
readChar(IEA_path, nchars = 256)
#> [1] ",,TIME,1971,2000\r\nCOUNTRY,FLOW,PRODUCT,,\r\nGhana,Production,Hard coal (if no detail),0,x\r\nGhana,Production,Brown coal (if no detail),0,x\r\nGhana,Production,Anthracite,..,0\r\nGhana,Production,Coking coal,..,0\r\nGhana,Production,Other bituminous coal,..,0\r\nGhana"
```


### Convert the IEA data into an R data frame (`iea_df()`)

Extended energy balance data can be pulled directly into an `R` data frame 
by the `iea_df()` function. 
`iea_df()` fixes the header shown in the figure above,
conveting it to a single row.
Furthermore, `iea_df()` deals with the IEA's indicators for 
not applicable values ("`x`"),
for unavailable values ("`..`"), and
for confidential values ("`c`").
(See "World Energy Balances: Database Documentation (2018 edition)" at
(http://wds.iea.org/wds/pdf/worldbal_documentation.pdf).)

`R` has three concepts that could be used for "`x`" and "`..`":
`0` would indicate value known to be zero.
`NULL` would indicate an undefined value.
`NA` would indicate a value that is unavailable.

In theory, mapping from the IEA's indicators to `R` should be done as follows:
"`..`" (unavailable) and "`c`" (confidential) would be converted to `NA` in `R`.
"`x`" (not applicable) would be converted to `0` in `R`.
"`NULL`" would not be used.
However, the IEA are not consistent with their coding. 
In some places "`..`" (unavailable) is used for not applicable values,
e.g. World Anthracite supply in 1971. 
(World Anthracite supply in 1971 is actually not applicable ("`x`"), 
because Anthracite was classified under "Hard coal (if no detail)" in 1971.)
On the other hand, "`..`" is used correctly for data in the most recent year 
when those data have not yet been incorporated into the database. 

In the face of IEA’s inconsistencies, 
the only rational way to proceed is to convert 
"`x`", "`..`", and "`c`" to "`0`".
`iea_df()` performs that task.


```r
IEA_data <- IEA_path %>% 
  iea_df()
IEA_data %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 5
#> $ COUNTRY <chr> "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana"…
#> $ FLOW    <chr> "Production", "Production", "Production", "Production", "Prod…
#> $ PRODUCT <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Ant…
#> $ `1971`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```


## Next steps

There are several reasons why the IEA extended energy balance
data are not convenient for immediate use.

1. Column titles are SHOUTING.
2. The `COUNTRY` column contains full (long) country names. 
It would be nicer if countries were identified by their [2- or 3-letter ISO codes](https://en.wikipedia.org/wiki/ISO_3166-1).
3. The demarcation between the supply and consumptions sides of the ledger is unclear to new users.
4. How data should be aggregated is unclear to new users.
5. The data are not in [tidy format](https://doi.org/10.18637/jss.v059.i10).

The `IEATools` package has functions to address each of these issues.


### Fix column titles (`rename_iea_df_cols()`)

To unshout the column titles, use the `rename_iea_df_cols()` function.


```r
IEA_data %>% 
  rename_iea_df_cols() %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 5
#> $ Country <chr> "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana"…
#> $ Flow    <chr> "Production", "Production", "Production", "Production", "Prod…
#> $ Product <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Ant…
#> $ `1971`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

Note that the example above uses the `rename_iea_df_cols()` function without arguments,
because the default arguments are correct for the extended energy balance data
as it arrives from the IEA.
However, both the old and new names can be specified as arguments.
If you despise both capitals letters and vowels, you could do the following.


```r
IEA_data %>% 
  rename_iea_df_cols(new_country = "cntry", new_flow = "flw", new_product = "prdct")  %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 5
#> $ cntry  <chr> "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana",…
#> $ flw    <chr> "Production", "Production", "Production", "Production", "Produ…
#> $ prdct  <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Anth…
#> $ `1971` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ `2000` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
```

The default arguments are consistent throughout the `IEATools` package.
Thus, it is recommended that you use the default argument values,
wherever possible.


### Change to 3-letter ISO country abbreviations (`use_iso_countries()`)

To reduce the length of character strings representing countries, 
the `use_iso_countries()` function replaces country character strings
with each country's 3-letter abbreviation.


```r
IEA_data %>% 
  rename_iea_df_cols() %>% 
  use_iso_countries() %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 5
#> $ Country <chr> "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "GHA"…
#> $ Flow    <chr> "Production", "Production", "Production", "Production", "Prod…
#> $ Product <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Ant…
#> $ `1971`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

If the 2-letter abbreviations are preferred, set the `iso_abbrev_type = 2`
as shown below.


```r
IEA_data %>% 
  rename_iea_df_cols() %>% 
  use_iso_countries(iso_abbrev_type = 2) %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 5
#> $ Country <chr> "GH", "GH", "GH", "GH", "GH", "GH", "GH", "GH", "GH", "GH", "…
#> $ Flow    <chr> "Production", "Production", "Production", "Production", "Prod…
#> $ Product <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Ant…
#> $ `1971`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```


### Remove aggregation and memo data (`remove_agg_memo_flows()`)

The IEA's extended energy balances contain many memos and aggregations in the data itself.
For the purposes of manipulations and calculations, 
it is often advisable to remove all memos and aggregations.
The `remove_agg_memo_flows()` function performs that task.


```r
IEA_data %>% 
  rename_iea_df_cols() %>% 
  filter(Flow == "Total primary energy supply") %>% 
  glimpse()
#> Observations: 136
#> Variables: 5
#> $ Country <chr> "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana"…
#> $ Flow    <chr> "Total primary energy supply", "Total primary energy supply",…
#> $ Product <chr> "Hard coal (if no detail)", "Brown coal (if no detail)", "Ant…
#> $ `1971`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# Total primary energy supply is an aggregation row,
# so its rows should be absent after calling remove_agg_memo_flows().
IEA_data %>% 
  rename_iea_df_cols() %>% 
  remove_agg_memo_flows() %>% 
  filter(Flow == "Total primary energy supply")
#> [1] Country Flow    Product 1971    2000   
#> <0 rows> (or 0-length row.names)
```


### Augment with ledger side and aggregation point information (`augment_iea_df()`)

The IEA's extended energy balance data can be confusing for first-time users to understand.
In particular, both 

(a) the demarcation between the supply and consumption sides of the ledger and
(b) how data should be aggregated

are unclear.

To clarify these issues and make later aggregation calculations possible,
call the `augment_iea_data()` function.
`augment_iea_df()` has many default arguments that work fine 
for as-delivered IEA extended energy balance data 
in kilotons of oil equivalent units.
`augment_iea_df()` adds new columns 
`Ledger.side`, `Flow.aggregation.point`, `Energy.type`, and `Unit`.


```r
IEA_data %>% 
  rename_iea_df_cols() %>% 
  augment_iea_df() %>% 
  glimpse()
#> Observations: 14,688
#> Variables: 11
#> $ Country                <chr> "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "…
#> $ Method                 <chr> "PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "PCM…
#> $ Energy.type            <chr> "E", "E", "E", "E", "E", "E", "E", "E", "E", "…
#> $ Last.stage             <chr> "Final", "Final", "Final", "Final", "Final", "…
#> $ Ledger.side            <chr> "Supply", "Supply", "Supply", "Supply", "Suppl…
#> $ Flow.aggregation.point <chr> "Total primary energy supply", "Total primary …
#> $ Flow                   <chr> "Production", "Production", "Production", "Pro…
#> $ Product                <chr> "Hard coal (if no detail)", "Brown coal (if no…
#> $ Unit                   <chr> "ktoe", "ktoe", "ktoe", "ktoe", "ktoe", "ktoe"…
#> $ `1971`                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ `2000`                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```


### Convert to a tidy data frame (`tidy_iea_df()`)

The [tidy format](https://doi.org/10.18637/jss.v059.i10) 
is a data frame where each datum is located on its own row
and columns provide metadata for each datum.
As delivered, the IEA's data are not tidy: 
years are spread to the right 
rather than being a single column.
The `tidy_iea_df()` function converts to a tidy format.
By default, `tidy_iea_df()` removes zeroes from the data frame,
thereby reducing memory footprint.


```r
Tidy_IEA_df <- IEA_data %>% 
  rename_iea_df_cols() %>% 
  use_iso_countries() %>% 
  remove_agg_memo_flows() %>% 
  augment_iea_df() %>% 
  tidy_iea_df()
Tidy_IEA_df %>% 
  glimpse()
#> Observations: 399
#> Variables: 11
#> $ Country                <chr> "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "GHA…
#> $ Method                 <chr> "PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "PCM…
#> $ Energy.type            <chr> "E", "E", "E", "E", "E", "E", "E", "E", "E", "…
#> $ Last.stage             <chr> "Final", "Final", "Final", "Final", "Final", "…
#> $ Year                   <dbl> 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971…
#> $ Ledger.side            <chr> "Supply", "Supply", "Supply", "Supply", "Suppl…
#> $ Flow.aggregation.point <chr> "Total primary energy supply", "Total primary …
#> $ Flow                   <chr> "Production", "Production", "Imports", "Import…
#> $ Product                <chr> "Primary solid biofuels", "Hydro", "Crude oil"…
#> $ Unit                   <chr> "ktoe", "ktoe", "ktoe", "ktoe", "ktoe", "ktoe"…
#> $ E.dot                  <dbl> 2088, 250, 916, 1, 21, 1, 18, -4, -4, -178, -2…
```


## The `load_tidy_iea_df()` function

For simplicity, 
all steps above are rolled into `load_tidy_iea_df()`.
By default, `load_tidy_iea_df()` 
loads the sample data bundled with the package and 
converts it to a tidy IEA data frame, 
assuming default arguments for all functions. 
But you can supply the path to any IEA data file
in the `file_path` argument to `load_tidy_iea_df()`.


```r
Simple <- load_tidy_iea_df()
Complicated <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
  system.file(package = "IEATools") %>% 
  iea_df() %>%
  rename_iea_df_cols() %>% 
  remove_agg_memo_flows() %>% 
  use_iso_countries() %>% 
  augment_iea_df() %>% 
  tidy_iea_df()
all(Simple == Complicated)
#> [1] TRUE
```

At this point, the integrity of the IEA data can be checked.


## Check data integrity

The IEA extended energy balance data are usually close to balanced, but not quite.
To check the integrity of the energy balances,
use the `calc_tidy_iea_df_balances()` function.
`calc_tidy_iea_df_balances()` adds several new columns to the tidy IEA data frame,
including:

* `supply_sum` (the sum of all flows on the supply side of the ledger),
* `consumption_sum` (the sum of all flows on the consumption side of the ledger),
* `supply_minus_consumption` (the difference between supply and consumption), 
* `balance_OK` (a logical telling whether the energy balance is acceptable), and 
* `err` (supply sum when `consumption_sum` is `NA` or 
  `supply_minus_consumption` when `consumption_sum` is not `NA`).


```r
Balances <- Tidy_IEA_df %>% 
  calc_tidy_iea_df_balances()
Balances %>% 
  glimpse()
#> Observations: 78
#> Variables: 12
#> $ Country                  <chr> "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "G…
#> $ Method                   <chr> "PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "P…
#> $ Energy.type              <chr> "E", "E", "E", "E", "E", "E", "E", "E", "E",…
#> $ Last.stage               <chr> "Final", "Final", "Final", "Final", "Final",…
#> $ Year                     <dbl> 1971, 1971, 1971, 1971, 1971, 1971, 1971, 19…
#> $ Product                  <chr> "Aviation gasoline", "Charcoal", "Crude oil"…
#> $ Unit                     <chr> "ktoe", "ktoe", "ktoe", "ktoe", "ktoe", "kto…
#> $ supply_sum               <dbl> 0, 119, 0, 236, 94, 219, 0, -1, 3, 18, 203, …
#> $ consumption_sum          <dbl> NA, 119, NA, 235, 95, 217, NA, NA, 3, 18, 20…
#> $ supply_minus_consumption <dbl> NA, 0, NA, 1, -1, 2, NA, NA, 0, 0, 1, 0, 0, …
#> $ balance_OK               <lgl> TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,…
#> $ err                      <dbl> 0, 0, 0, 1, -1, 2, 0, -1, 0, 0, 1, 0, 0, 0, …
Balances %>% 
  tidy_iea_df_balanced()
#> [1] FALSE
```

Notice that the IEA data are not quite energy balanced.
To fix the enery balance, use the `fix_tidy_iea_df_balances()` function.
`fix_tidy_iea_df_balances()` adjusts the `Statistical differences` flow
to achieve perfect energy balance.


```r
# Fix product-level balances within each country
Tidy_IEA_df %>% 
  fix_tidy_iea_df_balances() %>% 
  calc_tidy_iea_df_balances() %>% 
  glimpse()
#> Observations: 78
#> Variables: 12
#> $ Country                  <chr> "GHA", "GHA", "GHA", "GHA", "GHA", "GHA", "G…
#> $ Method                   <chr> "PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "P…
#> $ Energy.type              <chr> "E", "E", "E", "E", "E", "E", "E", "E", "E",…
#> $ Last.stage               <chr> "Final", "Final", "Final", "Final", "Final",…
#> $ Year                     <dbl> 1971, 1971, 1971, 1971, 1971, 1971, 1971, 19…
#> $ Product                  <chr> "Aviation gasoline", "Charcoal", "Crude oil"…
#> $ Unit                     <chr> "ktoe", "ktoe", "ktoe", "ktoe", "ktoe", "kto…
#> $ supply_sum               <dbl> 0, 119, 0, 235, 95, 217, 0, 0, 3, 18, 202, 8…
#> $ consumption_sum          <dbl> NA, 119, NA, 235, 95, 217, NA, NA, 3, 18, 20…
#> $ supply_minus_consumption <dbl> NA, 0, NA, 0, 0, 0, NA, NA, 0, 0, 0, 0, 0, N…
#> $ balance_OK               <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
#> $ err                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
```


# Conclusion

At this point, IEA extended energy balance 
data have been imported to `R` and
are ready to be used. 
The next step could be to specify some of the IEA energy flows.
See the [specify](specify.html) vignette for details.

