# Get channel data

Obtain channel information and data from given prefix and channel ID.

## Usage

``` r
get_channel(x, channel_id)
```

## Arguments

- x:

  path `prefix` specified in
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md),
  or `'nev/nsx'` object

- channel_id:

  integer channel number. Please be aware that channel number, channel
  ID, electrode ID refer to the same concept in 'Blackrock' 'NEV'
  specifications. Electrodes are not physical metals, they refer to
  channels for historical reasons.

## Value

A list containing channel data and meta information, along with the
enclosing 'NSx' information; for invalid channel ID, this function
returns `NULL`
