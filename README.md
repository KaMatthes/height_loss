# Associations between height loss from age 40 and health at age 69 in women and men born in the UK in 1946 

## Paper
submitted

## Data

The data is not public available und belongs to the National Survey of Health and Development (NSHD)
https://nshd.mrc.ac.uk/


## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- data_raw
+-- output

```

### `R` folder 

This folder contains all R scripts.

  - `data.R` : prepares the data for the analysis
  - `Table1.R` : code to create Table1 and values of proportion test
  - `Table2.R` : code for cross-protection calculation
  - `Figure1.R` : code for logistical regression models and Figure 1
  - `Supplement_Figure4.R` : code to create age distribution plot
 
  
### `data` folder

This folder contains the data created from  `data.R` 
  - `data_grippe.RData` 
  
### `data_raw` folder
This folder contains the raw data for the analysis:
-`Factory_Survey_Data.csv`

### `output` folder

This folder contains all outputs.

### `master.R` 

This skript contains information of the used R packages, R scripts, plotting parameters etc, please run first ` load("data/data_grippe.RData")`.

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
