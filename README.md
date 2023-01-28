
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

## Load data in R

### Example 1: load epoch from `NEV` comment data packet events

The following example loads the trial epoch table from `NEV` comment events. The `time_in_seconds` is the second of stimuli relative to `time_origin`.
Column `comment` is whatever comments sent by the task script (sent from `psychtoolbox` or related software that you use when collecting data).

```r
# prefix <- "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
nev <- readNSx::get_nev(prefix)

# `time_in_seconds` is relative to time-origin
nev$header_basic$time_origin
#>        Year       Month   DayofWeek         Day        Hour   
#>        2022           8           5          26          15   
#>      Minute      Second Millisecond 
#>           4          10         156 

readNSx::get_event(nev, "comment")
#>     timestamp packet_id char_set flag data   comment   event time_in_seconds 
#> 1      683033     65535        0    0  255  audio-ba comment        22.76777 
#> 2      753242     65535        0    0  255  video-ba comment        25.10807 
#> ...
```

### Example 2: get channel information and data

```r
# prefix <- "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"

# Gather information of channel 10
loaded <- readNSx::get_channel(prefix, channel_id = 10)

# Get NSx configurations, 
loaded$nsx
#> Basic header information (NSx):
#>   Internal type: NEURALCD
#>   Channel count: 152
#>   Sample rate: 2000 Hz
#>   Time origin: 2022-08-26 15:04:10 158ms
#> Extended header information (NSx):
#>   - CC (152 x 16, channels: 1-152): type, electrode_id, electrode_label, ...
#> Cache status:
#>   Prefix: ...
#>   Number of partitions: 1


# E.g. number of partitions (i.e. unpaused continuous recordings)
loaded$nsx$nparts
#> 1

# Get channel information
loaded$channel_info
#>    type electrode_id electrode_label physical_connector connector_pin
#> 10   CC           10        RA10-010                  1            10
#>    min_digital_value max_digital_value min_analog_value
#> 10            -32764             32764            -8191
#>    max_analog_value units high_freq_corner high_freq_order
#> 10             8191    uV              300               1
#>    high_freq_type low_freq_corner low_freq_order low_freq_type
#> 10              1         7500000              3             1
#>    sample_rate_signal sample_rate_timestamp which_nsp    filename
#> 10               2000                 30000         3 RA10-010.h5

# Get channel data
channel_signal <- loaded$channel_detail$part1$data; channel_signal
#> Class: H5D
#> Dataset: /data
#> Filename: ...
#> Access type: H5F_ACC_RDONLY
#> Datatype: H5T_IEEE_F64LE
#> Space: Type=Simple     Dims=798264     Maxdims=Inf
#> Chunk: 16384

# Get the actual numbers
channel_signal[]
```

Please use square bracket `channel_signal[]` to load the data into the memory.

## Anatomy of imported files

The imported file paths will start with `prefix`. In the following context, I'll use `<prefix>` to represent specified in the function `import_nsp`.

```
partition_info         - Name of continuous recording within the block, 
                          sample rates, starting time per partition per NSx
<prefix>_scans         - Basic information for current block
<prefix>_channels      - Electrode channel information ( ID, Label, ... )
<prefix>_events/       - NEV setting headers and data packets (events)
  - DIGLABEL           - Digital input setup
  - NEUEVLBL           - Channel labels
  - NEUEVWAVE          - Spike waveform settings
  - ...                - Other settings
  - event-***          - Data packets (digital inputs, comments...)
  - waveforms.h5       - Spike waveforms & cluster
<prefix>_ieeg          - NSx data folder
  - configurations.rds - NSx basic headers (versions, number of partitions, ...)
  - partition_info     - Continuous recording duration, start time, sample rates
  - nsx_summary.rds    - Internally used
  - part1/, part2/, ...- Channel folder
    - XXXX-001.h5      - Channel data file, each file correspond to a channel.
    - XXXX-002.h5         The file name ALWAYS ends with channel ID.
    - ...                 Each HDF5 file contains a "meta" and a "data" part,
                            "meta": JSON string of channel information
                            "data": numerical signal voltage data (in `uV`)
```

The following file types will be generated:

* `.h5` (`HDF5` file): common file format that can include one or more data within a single file. 
  - `Matlab`: use `h5disp` to get enclosing data names; use `h5read` to read specific data
  - `Python`: use `h5py` package to load the files
  - `R`: if you want to load them individually, use `raveio::h5_names` to get enclosing data names; use `raveio::load_h5` to read specific data. `readNSx` also provides high-level functions to load them (see `?readNSx::get_channel`).
* `.tsv` (tab-separated values file): plain text files that can be easily read by many languages. You can open them in `Microsoft Office: Excel`.
* `.rds` (R object file): can only be read from R, internally used by `readNSx` to store data objects. Users do not need to read from these files (but also do NOT delete them, or `readNSx` will break). They serve as redundant files in case the `tsv` files are altered accidentally (for example, `Excel` might alter data formats automatically). If you see an `.rds` file sharing the same name as `.tsv`, they share the same information. 

