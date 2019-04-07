# IEATools 0.1.6 (2019-04-06)

* Updated all three vignettes to reflect the new functions and demonstrate their use.
* New function `augment_iea_df()` now removes ` (energy)`, ` (transf.)`, and ` (transformation)` suffixes 
  from the `Flow` column after adding the `Flow.aggregation.point` column.
* No longer removing countries that lack a 2-letter ISO abbreviation.
* Added `prep_psut()` function to bundle several data munging steps into one.
* Added `production_to_resources()` function to convert `Production` to `Resources (product)`.
* Added `specify_primary_production()` function to fix primary production issues in the IEA data.
* Added constants `coal_and_coal_products`, `oil_and_oil_products`, `renewable_products`, `biofuel_and_waste_products`.
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