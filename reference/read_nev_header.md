# Read 'NEV' headers

Reads the basic and extended headers of a 'NEV' file, such as the
channel labels and waveform settings, without reading the data packets
and without writing any file. Unlike
[`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md), the
time needed does not grow with the length of the recording.

## Usage

``` r
read_nev_header(path)
```

## Arguments

- path:

  path to the 'NEV' file

## Value

A list of `header_basic` (file settings such as the time resolution and
the data packet size) and `header_extended` (one data frame per extended
header type, for example `NEUEVLBL` for channel labels and `NEUEVWAV`
for waveform settings). See
[`get_specification`](http://dipterix.org/readNSx/reference/get_specification.md)
for the fields.

## Examples

``` r

# Please get your own sample data first. This package does not
# provide sample data for privacy and license concerns :)

if(interactive() && file.exists("sampledata.nev")) {

header <- read_nev_header("sampledata.nev")

# channel labels
header$header_extended$NEUEVLBL

}
```
