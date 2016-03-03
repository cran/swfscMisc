# swfscMisc


## Changes in v 1.1

* Added `autoUnits` function
* Changed default arguments for `lat.range` and `lon.range` to `NULL` in `sample.map`. If not specified, the ranges will be set to the ranges of the `lat` and `lon`.


## Changes in v 1.0.9

* Added `transparent` function
* Changed distance and destination functions to accept partial matches for method 
of calculation, type of surface, and units


## Changes in v 1.0.8

* Fixed `das.read` to handle errors in position and suppress warnings about NAs
during numerical conversions.
* Fixed `das.map` to remove records with no position


## Changes in v 1.0.7

* Changed isBetween to accept a vector of numbers


# Changes in v 1.0.6

* Added NEWS.md
* Added `diversity` function (moved from strataG package)
* Added `isBetween` function to test if a number is between two numbers


