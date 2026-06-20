# Replication Materials

## Overview

This replication package accompanies the paper:

**Nonmarital Childbearing and Family Structure Among Asian Americans: The Role of Socioeconomic Status, Ethnicity, and Immigration** published in RSF: The Russell Sage Foundation Journal of the Social Sciences June 2026, 12 (3) 145-171; DOI: https://doi.org/10.7758/RSF.2026.12.3.07

**Authors:** Fumiya Uchikoshi (corresponding author) and Airan Liu

This replication package contains the data construction, statistical analysis, and figure-generation code used in the paper. The project combines data from the U.S. Natality Detail Files (NVSS) and the Current Population Survey Annual Social and Economic Supplement (CPS-ASEC).

Because the underlying data are publicly available but are not redistributed with this replication package, users must obtain the original datasets separately before running the code.

Users may find it convenient to open the project through `Replication.Rproj`, which establishes the project working directory and facilitates execution of the R scripts. A master script (`Master.do`) is also provided to coordinate the Stata replication process and can be used to run the analyses in sequence rather than executing individual scripts separately.

## Data Sources

### NVSS Natality Data

The natality data used in this study are derived from the National Vital Statistics System (NVSS) Natality Detail Files. These data can be obtained through the National Bureau of Economic Research (NBER) Vital Statistics Natality Birth Data archive:

https://data.nber.org/nvss/

Users should download the natality files corresponding to the years used in the analysis and place them in the `Data/NVSS/` directory.

### CPS-ASEC Data

The CPS Annual Social and Economic Supplement (ASEC) data used in this study can be obtained from IPUMS CPS:

https://cps.ipums.org/

Users should create and download the appropriate IPUMS CPS extracts and place the resulting files in the `Data/CPS-ASEC/` directory.

---

## Folder Structure

```text
Replication.Rproj

Master.do 

Code/
├── 1.NVSS_DataConst.do
├── 2.NVSS_Regression.do
├── 3.NVSS_Prediction.R
├── 4.CPS_ASEC_DataConst.R
├── 5.CPS_ASEC_Regression.do
└── 6.CPS_ASEC_Prediction.R

Data/
├── CPS-ASEC/
└── NVSS/

Results/
├── Figure1.pdf
├── Figure2.pdf
├── Figure3.pdf
├── Figure4.pdf
├── Figure5.pdf
├── FamilyStructure/
└── NMC/
```

---

## Code Files

### NVSS Analysis

#### `NVSS_DataConst.do`

Constructs the analytic NVSS natality dataset and generates variables used in the nonmarital childbearing analyses.

#### `NVSS_Regression.do`

Estimates regression models using the constructed NVSS dataset and produces model output used in the paper.

#### `NVSS_Prediction.R`

Generates predicted probabilities and figure inputs from NVSS regression results.

### CPS-ASEC Analysis

#### `CPS_ASEC_DataConst.R`

Constructs the CPS-ASEC analytic dataset and harmonizes variables used in the family structure analyses.

#### `CPS_ASEC_Regression.do`

Estimates regression models using the CPS-ASEC analytic dataset.

#### `CPS_ASEC_Prediction.R`

Generates predicted probabilities and figure inputs from CPS-ASEC regression results.

---

## Replication Workflow

The analyses should be run in the following order.

### Nonmarital Childbearing (NVSS)

1. `NVSS_DataConst.do`
2. `NVSS_Regression.do`
3. `NVSS_Prediction.R`

### Family Structure (CPS-ASEC)

1. `CPS_ASEC_DataConst.R`
2. `CPS_ASEC_Regression.do`
3. `CPS_ASEC_Prediction.R`

Running these scripts sequentially will reproduce the analytical results and figures reported in the paper.

---

## Tables

The following output files correspond to the tables reported in the paper.

| Paper Table | Corresponding Output File                                                  |
| ----------- | -------------------------------------------------------------------------- |
| Table 1     | Descriptive statistics files generated from the NVSS and CPS-ASEC analyses |
| Table 2     | `Logit_NMC_Asian_19Jun2026.csv`                                            |
| Table 3     | `Logit_NMC_Asian_Nativ_19Jun2026.csv`                                      |
| Table 4     | `FamSt_Logit_Asian_19Jun2026.csv`                                          |
| Table 5     | `FamSt_Logit_Asian_generation_19Jun2026.csv`                               |

These files are generated during the replication process and correspond to the statistical results presented in the manuscript.

---

## Figures

The following figure files are included in the replication package:

* `Figure1.pdf`
* `Figure2.pdf`
* `Figure3.pdf`
* `Figure4.pdf`
* `Figure5.pdf`

These files correspond to the figures reported in the paper.

---

## Software Requirements

The analyses were conducted using:

* Stata
* R
* RStudio (optional; project file provided as `Replication.Rproj`)

Users should ensure that all required R packages and Stata commands referenced in the scripts are installed before running the replication files.

---

## Contact

For questions regarding the replication materials, please contact the corresponding author Fumiya Uchikoshi (uchikoshi@alumni.princeton.edu).
