# regone
Code for project one, scenario one in SF2930 Regression Analysis at KTH Royal Institute of Technology. 

Dataset must be of type `tibble`.

Note: Applicable to other datasets with numeric predictors. Change the function ìnput `dv` in `RunFullAnalysis`.

## Running with `bodyfatmen.csv`dataset

  1. Download the `bodyfatmen.csv` file from the course page, and place it in a data folder in the parent directory.
  2. Import `devtools` and run `devtools::install_github("itslwg/regone")`.
  3. Run `regone::RunProject()`.
  
## Running with other dataset

  1. Import `devtools` and run `devtools::install_github("itslwg/regone")`.
  2. Run `regone::RunFullAnalysis()`, changing the function input `dv` to the label of your dependent variable.
