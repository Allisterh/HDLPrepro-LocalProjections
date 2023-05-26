
<!-- README.md is generated from README.Rmd. Please edit that file -->

## HDLPrepro: Reproducing Adamek, Smeekes, Wilms (2023)

This package contains code and datasets to reproduce the simulations and
applications in Adamek, Smeekes, Wilms (2023) Local Projection Inference
in High-Dimensions.

## Installation

This package can be installed easily with the devtools package:

``` r
utils::install.packages("devtools")
```

You can then install the HDLPrepro package from the GitHub depository
directly by running:

``` r
devtools::install_github("RobertAdamek/HDLPrepro")
```

This package also makes heavy use of our package `desla`. The latest
version of this package can be installed from the GitHub depository by
running

``` r
devtools::install_github("RobertAdamek/desla")
```

## Usage

In addition to a collection of functions and datasets, this package
contains a several R scripts which reproduce different parts of our
paper. These scripts can be found in the `inst/replication_scripts`
folder of the package. They are easily accessible from the GitHub page
<https://github.com/RobertAdamek/HDLPrepro>, or by unpacking the package
tarball with `untar()`.

These scripts make use of several packages other than `HDLPrepro` and
`desla`. For completeness, we list all used packages here, as well as
the package versions with which we ran the code ourselves:

- `desla` (0.2.0)
- `dplyr` (1.1.2)
- `ggpattern` (1.0.1)
- `ggplot2` (3.4.2)
- `ggpubr` (0.6.0)
- `HDLPrepro` (1.0.0)
- `parallel` (4.2.2)
- `readxl` (1.4.1)
- `reshape2` (1.4.4)

We expect these packages to remain backwards compatible, and the most
up-to-date versions can be installed with

``` r
utils::install.packages(pkgs = c("dplyr", "ggpattern", "ggplot2", "ggpubr", "parallel", "readxl", "reshape2"))
```

The scripts included are as follows:

- Simulations in Section 3.1: *Sparse Structural VAR Model*
  - `running_simulation3_1.R`: Runs the simulation and saves
    intermediate files in a user-chosen folder.
  - `plotting_simulation3_1.R`: Processes these files and generates the
    plots in our paper.
- Simulations in Section 3.2: *Empirically Calibrated Dynamic Factor
  Model*
  - `processing_FREDMD.R`: Processes the data obtained from the FRED-MD
    database. It includes our data transformations which differ from the
    “default” choices, and our classification of “fast” and “slow”
    variables.
  - `calibrating_DFM3_2.R`: Uses the processed FRED-MD data to estimate
    dynamic factor models and compute their implied true impulse
    response functions.
  - \`running_simulation3_2.R\`\`: Uses the estimated DFMs to simulate
    data and estimate the impulse responses. Saves intermediate files in
    a user-chosen folder.
  - `plotting_simulation3_2.R`: Processes the simulation files and
    generates the plots in our paper.
- Application in Section 4.1: *Impulse Responses to a Shock in Monetary
  Policy*
  - `application4_1.R`: Estimates the models and plots the results,
    using the same FRED-MD processed data as that used in Section 3.2.
- Application in Section 4.2: *Impulse Responses to a Shock in
  Government Spending*
  - `processing_R&Z_data.R`: Processes the data obtained from the
    website of Valerie A. Ramey, relating to the paper Ramey and Zubairy
    (2018).
  - `application4_2.R`: Estimates the models and plots the results.

## References

Adamek, R., S. Smeekes, and I. Wilms (2023). Local Projection inference
in High Dimensions. *The Econometrics Journal*, Forthcoming.

McCracken, M. W. and S. Ng (2016a). FRED-MD: a monthly database for
macroeconomic research. *Journal of Business & Economic Statistics 34*,
574–589.

Ramey, V. A. and S. Zubairy (2018). Government spending multipliers in
good times and in bad: evidence from US historical data. *Journal of
Political Economy 126*, 850–901
