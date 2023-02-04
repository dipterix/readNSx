
# readNSx

<!-- badges: start -->
[![license](https://img.shields.io/badge/license-MPL--2.0%20%2B%20file%20LICENSE-blue)](https://github.com/dipterix/readNSx/blob/main/LICENSE)
[![CRAN status](https://www.r-pkg.org/badges/version/readNSx)](https://CRAN.R-project.org/package=readNSx)
[![R-check](https://github.com/dipterix/readNSx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/readNSx/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `readNSx` is to read in `Blackrock-Microsystem` files (`.nev`, `.nsx`) and save the information to common formats that are well-supported by R, Python, Matlab.

## Installation

The package is on `CRAN` soon. Install it via 

```r
install.packages("readNSx")
```

You can install the nightly development version of `readNSx` from [r-universe](https://dipterix.r-universe.dev/readNSx)

``` r
# Enable repository from dipterix
options(repos = c(
  dipterix = 'https://dipterix.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install readNSx in R
install.packages('readNSx')
```

## How to use `readNSx`

[Click here](https://dipterix.org/readNSx/articles/read-nev-nsx-data.html) to read "not that detailed" manual including usage, anatomy of `readNSx`. 

### Import into RAVE

To import the data into `RAVE (R Analysis and Visualization of iEEG)`, use the following code as an example.

``` r
readNSx::import_nsp(
  path = "~/EMU_RAW/EMU-008_sub-YAB_task-congruency_run-01_NSP-1.nev", 
  prefix = "~/rave_data/raw/YAB/block008", 
  exclude_events = "spike", partition_prefix = "_part"
)
```

The raw data is stored as `EMU-008_sub-YAB_task-congruency_run-01_NSP-1.nev` along with `ns3` and `ns5`. The data will be written to `RAVE` raw-data path under `~/rave_data/raw/`, as `YAB/block008_part1`, `YAB/block008_part2`, ... (one block of data may contain multiple segments of continuous recordings). The above example also avoids reading default `spike-waveforms`. Simply set `exclude_events=NULL` will enable default spike clusters. If you just want to import certain `NSx` files (for example, only `ns3`), then check `exclude_nsx` parameter.

### Import to `BIDS`-like format

`Blackrock-Microsystem` data is incompatible with `BIDS`. It lacks several critical information and require manually edits. However, you may use `import_nsp` to import into `BIDS`-like format. 

```r
readNSx::import_nsp(
  path = "~/EMU_RAW/EMU-008_sub-YAB_task-congruency_run-01_NSP-1.nev", 
  prefix = file.path(
    "~/BIDSRoot/MyDataSet/sub-YAB/ses-008/ieeg/",
    "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
  ), 
  exclude_events = "spike", partition_prefix = "/part"
)
```


## License

(This is not legal notice. Please seek for professional advice)

The source code of `readNSx` is freely available for educational use. Some components might subject to `Blackrock` copyright. Please contact `Blackrock` for permissions if your software is not free.

`readNSx` is released under `MPL-2.0` license with copyright information. To link `readNSx` in your project (e.g. via R `library(readNSx)` function), you do NOT need to change your license (even for proprietary projects, this makes me prefer `MPL-2.0` to other strong copyleft licenses as `GPL`) other than including the copyright information when redistributing (see `LICENSE` file). 

In some rare cases, if you redistribute source code, either modified or as-is outside of your organization, you must release the code under `MPL-2.0` license. The license file explicitly states that the source code is incompatible with any other license (including `GPL`, see `Exhibit B - “Incompatible With Secondary Licenses” Notice` in the license file).
