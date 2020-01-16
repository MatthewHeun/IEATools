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