# Read `'BCI2000'` recording data

Read `'BCI2000'` recording data

## Usage

``` r
read_bci2000_header(file)

read_bci2000(file)
```

## Arguments

- file:

  path to the recording data

## Value

Parsed signal data

## Examples

``` r
# Package comes with sample data
file <- system.file("samples", "bci2000_sample.dat", package = "readNSx")
result <- read_bci2000(file)

print(result)
#> <BCI2000 Recording Data>
#> HeaderLen=  8189 SourceCh= 64 StatevectorLen= 15
#> [State Definitions]:
#>   <BCI2000 State Definitions>
#>     [BCIStateDef: Running    ] Len=8, ByteLoc=0, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Active ] Len=8, ByteLoc=1, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: SourceTime ] Len=16, ByteLoc=2, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: RunActive  ] Len=8, ByteLoc=4, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Recording  ] Len=8, ByteLoc=5, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: IntCompute ] Len=8, ByteLoc=6, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: ResultCode ] Len=8, ByteLoc=7, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusTime   ] Len=16, ByteLoc=8, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Feedback   ] Len=8, ByteLoc=10, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: RestPeriod ] Len=8, ByteLoc=11, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusCode   ] Len=8, ByteLoc=12, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusBegin  ] Len=8, ByteLoc=13, BitLoc=0, DefaultValue=0
#>   <BCI2000 Data Definitions>
#>     Data Type: int16
#>     # of Channels: 64
#>     Each Sample Contains: 143 Bytes
#>     # of Samples read: 500
#> [Parameter Definitions]
#>   <BCIParameters>
#>     - Storage
#>     - Filtering
#>     - Visualize
#>     - Source
#>     - UsrTask
#>     - System
#>     - save_bcidat
#>     - Statistics
#>     - MEMFilter
#> [Enclosing Items]:
#>   $header_basic          - BCI2000 basic header information
#>   $parameters            - Recording parameters
#>   $states                - A list containing state definitions and state data
#>   $signals               - Time x Channel signal matrix

# Notive: v1.0 and v1.1 are different, but all in `Source` section
# sample rate
result$parameters$Source$SamplingRate$value
#> [1] 160

# Signal data 64 channels x 500 time-points
dim(result$signals)
#> [1]  64 500
```
