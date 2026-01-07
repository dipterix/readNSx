# Import signal data from 'Blackrock-Microsystems' data files

Please use `import_nsp` to import 'NEV' and 'NSx' files.

## Usage

``` r
import_nsp(
  path,
  prefix = NULL,
  exclude_events = "spike",
  exclude_nsx = NULL,
  verbose = TRUE,
  partition_prefix = "/part"
)
```

## Arguments

- path:

  path to 'NEV' or 'NSx' files

- prefix:

  path prefix to save data files into

- exclude_events:

  exclude one or more 'NEV' data events, choices are `'spike'`,
  `'video_sync'`, `'digital_inputs'`, `'tracking'`, `'button_trigger'`,
  `'comment'`, `'configuration'`; default is `'spike'` since the spike
  data takes very long time to parse, and most users might still use
  their own offline spike-sorting algorithms.

- exclude_nsx:

  excluded 'NSx' types, integer vectors from 1 to 9; for example,
  `exclude_nsx=c(1,2)` will prevent `'ns1'` and `'ns2'` from being
  imported

- verbose:

  logical or a progress object: when logical, `verbose` indicates
  whether to print the progress as messages; when `verbose` is a
  progress object, this object must have `inc` method; examples are
  `Progress` in the `shiny` package, `progress2` from `dipsaus`, or
  `shiny_progress` from `shidashi`

- partition_prefix:

  additional prefix to the data partition; default is `"/part"`

## Value

A list of configurations, see
[`get_specification`](http://dipterix.org/readNSx/reference/get_specification.md)
for what's contained.

## Examples

``` r
# Please get your own sample data first. This package does not
# provide sample data for privacy and license concerns :)

if(interactive() && file.exists("sampledata.nev")) {

library(readNSx)

# ---- Import for the first time --------------------------------
import_nsp(
  path = "sampledata.nev",
  prefix = file.path(
    "~/BIDSRoot/MyDataSet/sub-YAB/ses-008/ieeg/",
    "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
  ),
  exclude_events = "spike", partition_prefix = "/part"
)

# ---- Load header information --------------------------------
prefix <- "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
nev <- get_nev(prefix)
ns3 <- get_nsx(prefix, which = 3)

# get nev from nsx, or nsx from nev
get_nev(ns3)
get_nsx(nev, which = 5)

# ---- Load channel data
result <- get_channel(prefix, channel_id = 10)
channel_signal <- result$channel_detail$part1$data
channel_signal[]

}

```
