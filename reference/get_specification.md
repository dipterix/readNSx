# Get '.nev' or 'nsx' specification

Get '.nev' or 'nsx' specification

## Usage

``` r
get_specification(version, type = c("nev", "nsx"))
```

## Arguments

- version:

  either character string or a vector of two integers; for example,
  `"2.2"`, `"2.3"`, `c(3, 0)`. Currently only these three versions are
  supported since I was unable to find any other versions. Please file
  an issue ticket if others versions are desired.

- type:

  file type; choices are `'nev'` or `'nsx'`

## Value

The file specification as a list. The specification usually contains
three sections: basic header (fixed length), extended header
(dictionary-style), and data packets (data stream). The specification is
used to parse the data files.

## 'NEV' Data

A 'NEV' object consists of three sections:

Section 1 contains basic information such as the time-origin of all the
time-stamps, the time-stamp sampling frequency, data packets sizes.

Section 2 is extended header containing the configurations of channels,
digital signals, etc. For any data packets in section 3, there should be
at least one table in this section describing the settings.

section 3 is a collection of event packets such as digital signal inputs
( most likely to be used at version 2.2 or by 'Ripple'), spike waveform,
comments (sometimes storing epoch information), etc.

Please be aware that while most common entries can be found across
different file versions, some entries are version-specific. If you are
making your script general, you need to be very careful handling these
differences. For more information, please search for the data
specification manual from the 'Blackrock-Microsystems' website.

## 'NSx' Data

A 'NSx' file refers to the data files ending with `'ns1'` through
`'ns9'`. Common types are `'ns2'` (sampling at 1000 Hz), `'ns3'`
(sampling at 2000 Hz), and `'ns5'` (sampling at 30,000 Hz).

A 'NSx' file also consists of three sections. Section 1 contains basic
information such as the time-origin of all the time-stamps, sampling
frequencies, and channel counts within the file. Please be careful that
item `time_resolution_timestamp` is not the sampling frequency for
signals. This item is the sampling frequency for time-stamp. To obtain
the signal sample rate, divided `time_resolution_timestamp` by `period`.
For example, `'ns3'` usually has time-stamp resolution `30,000` and
`period=15`, hence the signal sample rate is `30000/15=2000Hz`.

Section 2 usually contains one and only one channel table of which the
number of rows should coincide with number of channels from section 1.
Other information such as channel labels, physical connectors, pins,
units, filter settings, digital-to-analog conversion are also included.
Since `readNSx` always attempts to convert signals in 'volts' or
'milli-volts' to 'micro-volts', the `'units'` column might be different
to what's actual recorded in the 'NSx' file headers.

Section 3 contains partitions of continuous recording. When
imported/loaded from `readNSx`, the digital signals are always converted
to analog signals with 'micro-volts' unit. Please use
[`get_channel`](http://dipterix.org/readNSx/reference/get_channel.md) to
get the channel data.

## Examples

``` r
get_specification(version = c(2,3), type = "nev")
#> <Blackrock [nev] specification version [2.3]>
#> Section 1: Header Basic Information.
#>   - file_type [type: string, size: 8]
#>   - file_spec [type: uint8]
#>   - additional_flags [type: uint16]
#>   - bytes_in_headers [type: uint32]
#>   - bytes_in_data_packet [type: uint32]
#>   - time_resolution_timestamp [type: uint32]
#>   - time_resolution_samples [type: uint32]
#>   - time_origin [type: uint16]
#>   - application_to_create_file [type: string, size: 32]
#>   - comment [type: string, size: 256]
#>   - number_of_extended_headers [type: uint32]
#> Section 2: Header Extended Information.
#>   - ARRAYNME [type: packet, size: 32]
#>   - ECOMMENT [type: packet, size: 32]
#>   - CCOMMENT [type: packet, size: 32]
#>   - MAPFILE [type: packet, size: 32]
#>   - NEUEVWAV [type: packet, size: 32]
#>   - NEUEVLBL [type: packet, size: 32]
#>   - NEUEVFLT [type: packet, size: 32]
#>   - DIGLABEL [type: packet, size: 32]
#>   - VIDEOSYN [type: packet, size: 32]
#>   - TRACKOBJ [type: packet, size: 32]
#> Section 3: Data Packets.
#>   - 0 [type: packet, size: 20]
#>   - 1-2048 [type: packet, size: 16]
#>   - 65535 [type: comment_packet, size: 24]
#>   - 65534 [type: packet, size: 40]
#>   - 65533 [type: packet, size: 28]
#>   - 65532 [type: packet, size: 16]
#>   - 65531 [type: packet, size: 16]

get_specification(version = "3.0", type = "nsx")
#> <Blackrock [nsx] specification version [3.0]>
#> Section 1: Basic Header.
#>   - file_type [type: string, size: 8]
#>   - file_spec [type: uint8]
#>   - bytes_in_headers [type: uint32]
#>   - label [type: string, size: 16]
#>   - comment [type: string, size: 256]
#>   - period [type: uint32]
#>   - time_resolution_timestamp [type: uint32]
#>   - time_origin [type: uint16]
#>   - channel_count [type: uint32]
#> Section 2: Extended Headers.
#>   - CC [type: packet, size: 66]
#> Section 3: Data Packets.
#>   - data_header [type: packet, size: 13]
#> Section 3 (continued): Variable data points.
#>   - data_points [type: int16]
```
