# Revision history for `th-deepstrict`

## Unreleased

* Support GHC-9.12

## 0.2.0.0 -- 2024/07/16

* ExpectUnlifted is an option when specifying the parameters for a data type, meaning
   that the data type is deepstrict iff the parameter is unlifted.

* Also renamed the Strictness/ExpectedStrictness enum to more accurately reflect
   what it means.

## 0.1.1.0 -- 2024/03/26

* Support data and type families. 

## 0.1.0.0 -- 2024/03/18

* First version. Released on an unsuspecting world.
