# Get event data packets from 'NEV'

Get event data packets from 'NEV'

## Usage

``` r
get_event(x, event_type, ...)
```

## Arguments

- x:

  path `prefix` (see
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md)),
  or `'nev/nsx'` object

- event_type:

  event type to load, common event types are

  `'digital_inputs'`

  :   packet identifier 0

  `'spike'`

  :   packet identifier 1 to 10000 as of version 3.0

  `'recording'`

  :   packet identifier 65529 as of version 3.0, available after version
      3.0

  `'configuration'`

  :   packet identifier 65530 as of version 3.0, available after version
      3.0

  `'log'`

  :   packet identifier 65531 as of version 3.0, available after version
      3.0

  `'button_trigger'`

  :   packet identifier 65532 as of version 3.0, available after version
      3.0

  `'tracking'`

  :   packet identifier 65533 as of version 3.0, available after version
      3.0

  `'video_sync'`

  :   packet identifier 65534 as of version 3.0, available after version
      3.0

  `'comment'`

  :   packet identifier 65535 as of version 3.0, available after version
      2.3

- ...:

  pass to other methods

## Value

A data frame of corresponding event type, or `NULL` if event is not
found or invalid
