* `extend_to_useful()` now robust to cases where there is no EIOU,
  thanks to now using `matsindf::matsindf_apply()`.
* Now using `matsindf::matsindf_apply()` for `extend_to_useful()`.
  This change will enable better handling of cases where
  a country has no EIOU.
  The first use case will be bunkers.
* No longer cleaning matrices in `extend_to_useful_helper()`, 
  because some vectors may be the `0` vector and be eliminated.
* Added `replace_null_UR()` function to the workflow in the `prep_psut()` function.
* New function `replace_null_UR()` replaces missing or `NULL` 
  `R`, `U_feed`, `U_EIOU`, `U`, and `r_EIOU`
  with **0** matrices with appropriate row and column names.
  The replacements are built from **Y** and **V** matrices.
  The need for this functionality arises when
  the last stage is final energy 
  and imports (**V** matrix) 
  are the only source of an energy carrier that 
  is consumed in final demand (**Y** matrix).
  In that situation, the **R** and **U** matrices 
  will be missing, 
  and they can be replaced by **0** matrices with the right dimensions.
* In the specification process,
  the `Flow` for countries `World_X_bunkers` are now
  specified to be "International navigation" and "International aviation"
  to correspond with their domestic equivalents.
* New tests for new code.
    * Up to 833 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.55 (2021-07-21) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5119070.svg)](https://doi.org/10.5281/zenodo.5119070)

* Modifications to code and tests
  to accommodate World marine bunkers and World aviation bunkers being their own country.
* New tests for new features.
    * Up to 808 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.54 (2021-07-09) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5086372.svg)](https://doi.org/10.5281/zenodo.5086372)

* Modifications to enable per-machine
  final-to-useful efficiency data.
* New tests for new features.
    * Up to 790 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.53 (2021-06-08)

* The "Oil and gas extraction" activity is split in "Oil extraction",
  which extracts oil products, and "Natural gas extraction", which extracts natural gas.
* New `split_oil_gas_extraction_eiou()` function splits the EIOU of 
  the "Oil and gas extraction" industry into EIOU for the
  "Oil extraction" and for the "Natural gas extraction" activities.


# IEATools 0.1.52 (2021-05-07)

* Deleted an efficiency for LPG stoves in Ghana in 1971
  that should not have been in the GH-ZA-Efficiency-sample-2019.xlsx file.
* Moved A_B_data_full_2018_format_testing.csv to `inst/extdata`.
* `specify_primary_production()` updated so that all products coming from Resources
  are now called Product [from Resources]. Then, for each product, 
  a manufacturing industry takes these Product [from Resources] as inputs 
  and transforms them into actual Products.


# IEATools 0.1.51 (2021-04-11)

* Transition away from Travis to GitHub actions for continuous integration testing.
* No new tests.
    * Still at 766 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.50 (2021-03-31)

* Now more robust support for tidy (instead of wide-by-year) format efficiency data.
* Now more arguments are passed from top-level functions to lower-level functions,
  thereby allowing external callers to use non-default column names.
* Moving some code to new syntax for referring to column names passed as function arguments.
* New tests for new features.
    * Now up to 766 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.49 (2021-03-17)

* New constants: `industry_net_flows`, `transport_domestic_flows`, `fd_sectors`, `eiou_flows`, and `prim_agg_flows`
* Improving documentation of constants.


# IEATools 0.1.48 (2021-02-18)

* Minor issue fixed regarding the `coal_and_coal_products` constant.


# IEATools 0.1.47 (2021-02-16)

* New `specify_all()` function coded, so that it now splits non-specified
  industries, splits the IEA "Own use in electricity, CHP and heat plants"
  flow in main activity producer electricity, heat, and CHP plants, and so that
  a nuclear industry is added when needed.
* Sub-functions for the `specify_all()` function 
  are called within the `specify_tp_eiou()` function.
* These include the `gather_producer_autoproducer()`,
  the `route_pumped_storage()`, the `route_own_use_elect_chp_heat()`,
  the `add_nuclear_industry()`, and the `route_non_specified_flows()` functions.
*`specify_primary_production()` function changed, the product
  naming is now modified.
* `extract_S_units_from_tidy()` function hotfixed so that
  it also works when the `.tidy_iea_df` data frame already
  has a `matnames` column.


# IEATools 0.1.46 (2021-02-01)

* `calc_tidy_iea_df_balances` function updated
  so that it also accounts for balancing flows 
  when checking balances.
* New bug in `aggregate_regions()` hotfixed.
* New option for "Epsilon" matrices and 
  PSUT columns enables removal of some `Flow`s 
  for footprinting analyses (and maybe other work in the future).
* When `matnames` is present in `.tidy_iea_df`, 
  `add_psut_matnames()` now returns `.tidy_iea_df` unmodified.
  This new feature enables other functions for assigning matrix names.
* When all of `rownames`, `colnames`, `rowtypes`, and `coltypes` 
  are present in `.tidy_iea_df`,
  `add_row_col_meta()` now returns `.tidy_iea_df` unmodified.
  This new feature enables other functions for assigning row and column names
  and row and column types.
* New tests for the new features.
    * Now up to 675 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.45 (2020-12-28)

* Bug in `aggregate_regions()` hotfixed.
* Still 661 tests, coverage remains at 100%.


# IEATools 0.1.44 (2020-12-08)

* Moved `primary_aggregates_IEA()` and `finaldemand_aggregates_IEA()` 
  from the `Recca` package to `IEATools`.
  Functions renamed to remove the `_IEA` suffix.
  Hosting the functions in `Recca` must have made sense at some point.
  But now it is clear these functions should be in `IEATools`, 
  because they aggregate IEA-style tidy data frames.
* New tests for the new feature.
    * Now up to 661 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.43 (2020-12-05)

* `extend_to_useful()` now creates `r_EIOU` matrices
* New tests for the new feature.
    * Now up to 659 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.42 (2020-11-30)

* `prep_psut()` now creates `r_EIOU` matrices
* New tests for the new feature.
    * Now up to 658 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.41 (2020-11-25)

* Cleaned up (removed) unused arguments to `final_to_useful()`.
* Still at 656 tests, all passing.
* Test coverage remains at 100 %.


# IEATools 0.1.40 (2020-11-23)

* Now setting row name of column sums in `extend_to_useful_helper()`
  to get around a bug that occurs when a 1x1 matrix is column summed.
* The API of `extend_to_useful()` now assumes that 
  columns `C_Y`, `C_eiou`, and `eta_fu` are present in the incoming `.sutdata` 
  data frame.
  This change makes the API of `extend_to_useful()` more consistent with 
  the APIs of other functions in the package.
* Still at 656 tests, all passing.
* Test coverage remains at 100 %.


# IEATools 0.1.39 (2020-11-19)

* Fixed a dormant bug in `form_eta_fu_phi_u_vecs()` exposed by new code.
* `form_eta_fu_phi_u_vecs()` now accepts tidy data frames on input.
  This change allows better integration with the `SEAPSUTWorkflow` package.
* `form_C_mats()` now accepts tidy data frames on input.
  This change allows better integration with the `SEAPSUTWorkflow` package.
* Still at 656 tests, all passing.
* Test coverage remains at 100 %.


# IEATools 0.1.38 (2020-11-19)

* `prep_psut()` now returns a column for the `U` matrix 
  in addition to `U_feed` and `U_EIOU` columns.
* Completed the first draft of a vignette on moving
  from final energy to useful energy 
  at the last stage of an energy conversion chain.
* Still at 656 tests, all passing.
* Test coverage remains at 100 %.


# IEATools 0.1.37 (2020-10-07)

* New `aggregate_regions()` function that enables regional aggregation
  of a `.tidy_iea_df` based on a user-defined aggregation table.
* Underlying `read_aggregation_region_table()` and 
  `default_aggregation_region_table_path()` enables `aggregate_regions()`.
* Default IEA to Exiobase mapping provided as .xlsx file in the extdata folder.
* Now 656 tests, all passing.
* Test coverage remains at 100 %.


# IEATools 0.1.36 (2020-09-10)

* Fixed bug where missing eta_fu info caused failure.
  Needed to allow for an empty `eta_fu_table` in `complete_eta_fu_table()`.
* New tests for the fixed bug.
    * Now at 636 tests, all passing.
    * Test coverage remains at 100 %.


# IEATools 0.1.35 (2020-08-26)

* First version that works with full SEAPSUT workflow.
* Removed an erroneous check for
  energy industry own use in `specify_primary_production()`.
  In fact, we need to specify all cases,
  not only if energy industry own use is present.
  This change solves a bug in a `drake` workflow.
* Added spell checking to build process.


# IEATools 0.1.34 (2020-08-19)

* Updated documentation.


# IEATools 0.1.33 (2020-08-17)

* New tests for new features and old bugs.
    * Now at 634 tests, all passing.
    * Test coverage remains at 100 %.
* Change from `U_excl_EIOU` to `U_feed` everywhere.
* `fix_tidy_iea_df_balances()` now deals correctly with a no-row IEA data frame.
* Fixed a potential bug when deleting the `.err` column
  in fix_tidy_iea_df_balances.
* Fixed bugs with completion code.


# IEATools 0.1.32 (2020-08-13)

* `eta_fu_template()` now accepts tidy data frames.
* New tests for new features and old bugs.
    * Now at 611 tests, all passing.
    * Test coverage remains at 100 %.
* New function `check_fu_allocation_data()` verifies that all `Ef.product` and `Eu.product`
  entries are identical when `Machine` is `Non-energy`.


# IEATools 0.1.31 (2020-07-29)

* Adjusted tests for errors instead of warnings.
    * Now at 610 tests, all passing.
    * Test coverage remains at 100 %.
* `.tol` on allocation sums bumped from `1e-10` to `1e-9`, because some 
  errors are `3e-10`.
* Warnings are now errors with *much* more descriptive information.


# IEATools 0.1.30 (2020-07-24)

* New tests for new features and old bugs.
    * Now up to 611 tests, all passing.
    * Test coverage remains at 100 %.
* Fixed a bug where a completed final-to-useful allocation table 
  would have the `C_source` column, 
  which caused a problem when checking for completion of a final-to-useful efficiency table. 
* `which_quantity` argument to `complete_eta_fu_table()` now checked for validity.
* Added "vignettes/References.bib" file. References now display properly on readme page.
* New function `tidy_eta_fu_table()` does what the name suggests.
  This function (and previous new function `tidy_fu_allocation_table()`) 
  enable elimination of duplicated code in several places.


# IEATools 0.1.29 (2020-07-21)

* New tests for new features and old bugs.
    * Now up to 606 tests, all passing.
    * Test coverage remains at 100 %.
* `extract_S_units_from_tidy()` now correctly sets rowtype by default.
  rowtype was `Unit`, but it should have been `Product`.
* New function `tidy_fu_allocation_table()` makes an FU allocation table tidy if it is not.
* `year_cols()` now returns the "Year" column, if it exists.
* New function `meta_cols()` gives metadata columns in any IEA data frame.
* Refactored `complete_fu_allocation_table()` and `complete_eta_fu_table()` to use 
  `fu_allocation_table_completed()` and `eta_fu_table_completed()`.
* New function `eta_fu_table_completed()` tells when an FU Efficiency table is complete.
* New function `fu_allocation_table_completed()` that tells when an FU Allocation table is complete.
* Improvements to `switch-notation` documentation.
* Added constant `IEATools::fu_analysis_file_info`.


# IEATools 0.1.28 (2020-07-02)

* "Memo: Greenland", "Memo: Palestinian Authority", and "Memo: Uganda"
  are no longer included in constant `IEATools::aggregation_regions`, 
  because these regions are _not_ aggregations.


# IEATools 0.1.27 (2020-06-29)

* New tests for new features.
    * Now up to 567 tests, all passing.
    * Test coverage remains at 100 %.
* Added new function `remove_agg_regions()`
  that removes known aggregation regions from an IEA extended energy balances data frame.


# IEATools 0.1.26 (2020-06-27)

* New tests for new features.
    * Now up to 561 tests, all passing.
    * Test coverage remains at 100 %.
* Added functions to complete an FU Allocation table and an eta_FU table:
  `complete_fu_allocation_table()` and `complete_eta_fu_table()`.
* When generating an efficiency template with `eta_fu_template()`, 
  any Machine whose name begins with "Non-energy use" 
  is now given default efficiency (eta.fu) of 0.0001% (i.e., 1e-6) and 
  phi of 1.0.
* Non-energy use no longer excluded when generating FU Allocation templates.


# IEATools 0.1.25 (2020-06-19)

* "CHN" is now a synonym for "People's Republic of China".


# IEATools 0.1.24 (2020-06-17)

* Fixed a bug where "China (P.R. of China and Hong Kong, China)" 
  was not being coded to "CHN" in `use_iso_countries()`.
  The problem is that the `countrycode` package uses a different name from 
  the IEA extended energy balance database. 
  For now, I have coded 
  "China (P.R. of China and Hong Kong, China)" as "CHN".
  Another option would be to code it as "CHNHKG".


# IEATools 0.1.23 (2020-06-17)

* Fixed issue #5 from the PFU-Database repository.
  The issue involved `IEATools::eta_fu_template()`.
  The `eu_product` `ke` was not being subtracted, leaving
  duplicate "KE" products in `eu_product_sort_order`.
  

# IEATools 0.1.22 (2020-06-10)

* Fixed a bug in `despecify_col()`.
  `resources` and `production` were not correctly using the `tpes_flows` object.


# IEATools 0.1.21 (2020-05-14)

* Bug fixes in `extend_to_useful()`. 
  Importantly, an energy balance check is now performed 
  at the end of `extend_to_useful()`, and 
  a warning is emitted if the energy balance check fails.


# IEATools 0.1.20 (2020-05-01)

* Added additional tests for bug fixes and new features.
    * Now up to 500 tests, all passing.
    * Test coverage remains at 100 %.
* Added function `extend_to_useful()` and supporting functions
  `form_C_mats()` and `form_eta_fu_phi_u_vecs()`.
  These functions use a matrix method to move from last stage of final energy 
  to last stage of useful energy.
* Changed default number of rows in an allocation template from 3 to 4,
  per request from Zeke Marshall. 
  Note that the number of rows in the allocation template is adjustable 
  at the time the template is created using the `n_allocation_rows` argument
  to `fu_allocation_template()`.
* Added functions for row and column notation in lists:
  `switch_notation_byname()`, `arrow_to_paren_byname()`, and `paren_to_arrow_byname()`.
  These functions are like `*_byname` functions in the package
  `matsbyname`: they accept both single matrices and lists of matrices.
  In that way, they are amenable to pipeline calculations using the 
  `matsindf` package.
  Adding this feature exposed bugs in `matsbyname::setrownames_byname()` and 
  `matsbyname::setcolnames_byname()`. 
  The bugs in `matsbyname` have been fixed, so these features are now working.
* Added functions for row and column notation:
  `paren_to_arrow()`, `arrow_to_paren()`, and `switch_notation()`.


# IEATools 0.1.19 (2020-04-01)

* New function `form_eta_fu_phi_u_vecs()` which 
  creates column vectors of final-to-useful efficiencies (eta_fu)
  and exergy-to-energy ratios (phi_u)
  from an efficiency table.
* Added new test to hit code path for generating diagnostic information
  when FU Allocation table is mal-formed.
  Code coverage is back up to 100 % for testing.


# IEATools 0.1.18 (2020-03-31)

* New function `form_C_mats()` which makes a data frame of C matrices 
  (a.k.a. final-to-useful allocation matrices) and metadata 
  from an FU Allocation table.


# IEATools 0.1.17 (2020-03-25)

* Added several new member objects to standard names across the package:
    + `industry_flows`
    + `non_specified_flows`
    + `transformation_processes`
    + `row_col_types`
    + `psut_cols`
    + `mat_meta_cols`
* `extract_TK()` now accepts strings 
  formulated for "cooling" in addition to "heating."
  Thus, "LTC.15.C" is now accepted and parses correctly.


# IEATools 0.1.16 (2020-03-24)

* Now up to 444 tests, which all pass.
* `iea_file_OK()` now works on the 2018 IEA extended energy balance data.
* `sort_iea_df()` now works for specified IEA data frames.
  Internally, `sort_iea_df()` now uses `despecify_col()`.
* New function `despecify_col()` removes specification decoration from a column.
* Now using prepositions in specification strings.
* Specification strings are now given in a package constant (`specify_notation`).
  These constants are referred throughout the package.
* All constants are now lists of character strings
  instead of vectors
  to allow `$` referencing of specific list items.
* Added names to all constants for easier programming.


# IEATools 0.1.15 (2020-03-16)

* Now up to 429 tests!
* Added a `fix_iea_data` vignette.  See [Fixes for IEA Data](https://matthewheun.github.io/IEATools/articles/fix_iea_data.html).
* Did preparatory work for future `fix_*` functions, including
  the internal `do_fix()` function which will work for fixing data from any country in any years,
  so long as a replacement data frame is available.
* Added function `fix_GHA_industry_electricity()` which adds specificity to Ghana's industry electricity.
* Added function `replace_join()` to assist with IEA data fixes.
* Added named `iea_cols` object to simplify and standardize names for columns in IEA data frames,
  both tidy and wide.
* Added a "Sorting" section to the IEATools vignette.
  It describes the `sort_iea_df()` function.
* renamed `sort_tidy_iea_df()` --> `sort_iea_df()`, because 
  the function also sorts wide data frames.
* `sort_tidy_iea_df()` previously worked only with tidy data frames (with a column of years).
  `sort_tidy_iea_df()` now also works with wide data frames (with years spread to the right).
* Fixed a bug in `sort_tidy_iea_df()` that results in `NA` values in the `Last.stage` column.
* Added a function called `sort_tidy_iea_df()` which sorts tidy IEA data frames
  with default IEA row orders.
* Completed function called `fix_GHA_psb()` which 
  smooths Ghana's Primary solid biofuels data
  in the years 1991--1999.


# IEATools 0.1.14 (2020-03-11)

* Fixed an issue with argument names: overwrite --> overwrite_file. 
  The change was made in the `IEATools` package develop branch
  against which the latest version of the PFU-Database repository was built.
  However, the argument name change needs to be on the master branch of `IEATools`
  so that `install_github()` gets the updated argument name.
* Added stub functions to fix certain IEA data.
  This capability is in development and not yet working.
* Added a script to make internal data frames for fixing IEA data.
  This capability is in development and not yet working.


# IEATools 0.1.13 (2020-03-08)

* Added template tests for both 2018 and 2019.
* Added new sample allocation and F-->U efficiency tables
  for 2019 data.
* Rebuilt the sample template for F-->U allocation,
  because new categories of energy consumption were provided for ZAF EIOU.
* Wrote new tests for the new approaches to determining 
  "Transformation processes" and "Energy industry own use".
* Implemented a new way to identify "Transformation processes" and "Energy industry own use" rows.
  The new method works for both the 2018 and 2019 editions of the IEA data.
  The new method is based on finding breakpoints between sections of the table.
  The method relies on the fact that each country's rows are in exactly the same order.
  The validity of this assumption can be tested with `iea_file_OK()`.
* Added a new function `adjacent_rownums()` that identifies the row numbers where 
  two adjacent entries exist. 
  This function is used extensively when determining the 
  row ranges for "Transformation processes" and "Energy industry own use".
* Added function `sample_iea_data_path()` which takes a year (representing year of data release)
  as an argument.
  Default year is (now) 2019.  
  In subsequent years, I will change the default year to the most-recent year available.
  2018 is also supported.
  This function allows supporting and testing of previous years' data versions.
* Added `iea_file_OK()` which tells whether all countries in an IEA extended energy balance table
  have the same order for all rows.
* Added "Paper, pulp and printing" to the list of industries for 2019.
  (In the 2018 edition of the IEA's data, "Paper, pulp and print" was the name of this industry.)
* There is no longer an "Other" aggregation in the IEA database.
  One test that had expected an "Other" aggregation point in the database (using 2018 data)
  was failing, because the expectation is no longer met in the 2019 version of the database.
  The test has been changed to no longer expect an "Other" aggregation point.
* Focused some tests that assumed some FLOWs would end in 
  "(transf.)", "(transformation)", or "(energy)" on sample IEA data from 2018.  
  No FLOWs end with those strings in the 2019 data.
  There is no harm to leave the code that strips trailing "(transf.)", "(transformation)", and "(energy)".
* `augment_iea_df()` now recognizes "Final consumption not elsewhere specified" as an "other" flow.
  "Final consumption not elsewhere specified" (in the 2019 data) is a synonym for "Non-specified (other)" 
  (in 2018 and earlier editions of the IEA data).
* `augment_iea_df()` now recognizes "Industry not elsewhere specified" as an industry flow.
  "Industry not elsewhere specified" (in the 2019 data) is a synonym for "Non-specified (industry)" 
  (in 2018 and earlier editions of the IEA data).
* `iea_df()` now removes white space, even from "FLOW" entries that are quoted
  to protect commas.


# IEATools 0.1.12 (2020-02-26)

* `iea_df()` now reads files with a clean header line.
* Improved documentation on the energy balance functions, 
  especially `fix_tidy_iea_df_balances()`, 
  where clarifying examples are now provided.


# IEATools 0.1.11 (2020-02-14)

* `iea_df()` now deletes columns that end with `estimated_year`, whose default value is "E".
  This feature allows us to process IEA data files with column names such as "2014E".


# IEATools 0.1.10 (2020-01-22)

* Integrated with [codecov.io](http://www.codecov.io) by adding `codecov.yml` file to repository.
* Integrated with Travis by adding `.travis.yml` file to repository.
  Now, Travis builds the package automatically after each commit.
  In addition, test coverage is calculated after each build.
* Now at 100 % test coverage.
  Got to this point by adding new tests to ensure that all parts of the code are being hit.
  And by adjusting the regex strings in `extract_TK()`.


# IEATools 0.1.9 (2020-01-15)

* Added GitHub Pages website.


# IEATools 0.1.8 (2019-07-24)

* Efficiency template now includes energy flows into final-to-useful machines for each year,
  indicating to analysts which years need no efficiency or exergy-to-energy ratio.
* New `sort_by` argument for `eta_fu_template()` allows "useful_energy_type" and "importance".
  "useful_energy_type" sorts first by `md`, `light`, `ke`, and `heat`, 
  then by magnitude of energy flow into the machine.
  "importance" sorts by magnitude of energy flow into the machine only.
  Default is "useful_energy_type".
  

# IEATools 0.1.7 (2019-04-18)

* Allocation template now sorting rows in IEA order.
* All specified primary flows now consumed somewhere in the ECC.
* Now specifying the source of output flows from inserted machines
  at primary level.
  For example, inserting Coal mines into the ECC gives now results in 
  Hard coal produced by (Coal mines).  
  For example, 
  "Hard coal (if no detail)" --> "Hard coal (if no detail) (Coal mines)".
* Added `fu_allocation_template()` function that makes templates for 
  deciding allocations to final-to-useful machines in
  Final consumption or Energy industry own use.
* `specify_tp_eiou()` now aggregates inputs and outputs separately
  at the end of its specification process.
  Doing so avoids a situation where resource extraction industries
  would look like a Transformation process sink industry when 
  inputs and outputs were netted together.


# IEATools 0.1.6 (2019-04-06)

* Updated all three vignettes to reflect the new functions and demonstrate their use.
* New function `augment_iea_df()` now removes ` (energy)`, ` (transf.)`, and ` (transformation)` suffixes 
  from the `Flow` column after adding the `Flow.aggregation.point` column.
* No longer removing countries that lack a 2-letter ISO abbreviation.
* New function `prep_psut()` bundles several PSUT-related functions together.
* Added functions to generate PSUT matrices
* Added `production_to_resources()` function to convert `Production` to `Resources (product)`.
* Added `specify_primary_production()` function to fix primary production issues in the IEA data.
* Added constants `coal_and_coal_products`, `oil_and_oil_products`, `renewable_products`, `biofuels_and_waste_products`.
* Rename package from `IEAData` to `IEATools` to better reflect its purpose.
  Users can't get data from this package, anyway. 
  This package will do more than distribute data. 
  This package allows users to also manipulate data and extend to useful stage.
* Now includes sample data from 2 countries and 2 years.
* Converted to functions for all tasks so more general.
* Added function to create an S_units matrix from tidy data.


# IEAData 0.1.5 (2018-08-10)

* Trying again. Doesn't seem like data were pushed to GitHub.


# IEAData 0.1.4 (2018-08-10)

* Now includes "World" as a country.


# IEAData 0.1.3 (2018-07-29)

* New version of FixedHNFuels.tsv file from Noah Ver Beek.


# IEAData 0.1.2 (2018-07-27)

* Fixed bug in which data are replaced for Honduras.


# IEAData 0.1.1 (2018-07-27)

* Added fixes for Honduras data from Noah Ver Beek.


# IEAData 0.1.0 (2018-05-25)

* Initial release.
