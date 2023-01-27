
# readNSx

<!-- badges: start -->
[![R-check](https://github.com/dipterix/readNSx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/readNSx/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `readNSx` is to read in `Blackrock-Microsystem` files (`.nev`, `.nsx`) and save the information to common formats that are well-supported by R, Python, Matlab.

## Installation

The package will be on `CRAN` soon (in a week).

You can install the development version of `readNSx` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("dipterix/readNSx")
```

## Import into RAVE

To import the data into `RAVE (R Analysis and Visualization of iEEG)`, use the following code as an example.

``` r
readNSx::import_nsp(
  path = "~/EMU_RAW/EMU-008_sub-YAB_task-congruency_run-01_NSP-1.nev", 
  prefix = "~/rave_data/raw/YAB/block008", 
  exclude_events = "spike", partition_prefix = "_part"
)
```

The raw data is stored as `EMU-008_sub-YAB_task-congruency_run-01_NSP-1.nev` along with `ns3` and `ns5`. The data will be written to `RAVE` raw-data path under `~/rave_data/raw/`, as `YAB/block008_part1`, `YAB/block008_part2`, ... (one block of data may contain multiple segments of continuous recordings). The above example also avoids reading default `spike-waveforms`. Simply set `exclude_events=NULL` will enable default spike clusters. If you just want to import certain `NSx` files (for example, only `ns3`), then check `exclude_nsx` parameter.

## Import to `BIDS`-like format

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

## Anatomy of imported files

The imported file paths will start with `prefix`. In the following context, I'll use `<prefix>` to represent specified in the function `import_nsp`.

```
partition_info.tsv      - Name of continuous recording within the block, 
                          sample rates, starting time per partition per NSx
<prefix>_scans.tsv      - Basic information for current block
<prefix>_channels.tsv   - Electrode channel information ( ID, Label, ... )
<prefix>_events         - 
```
