# Compare new chunked read_nsx with legacy version

Helper function to validate the new chunked reading implementation by
comparing results with the legacy version that reads all data at once.

## Usage

``` r
compare_nsx_methods(
  path,
  exclude_events = "spike",
  exclude_nsx = NULL,
  partition_prefix = "/part",
  tolerance = 1e-10,
  verbose = TRUE
)
```

## Arguments

- path:

  path to 'NEV' or 'NSx' files

- exclude_events:

  passed to
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md)

- exclude_nsx:

  passed to
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md)

- partition_prefix:

  passed to
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md)

- tolerance:

  numeric tolerance for comparing signal values (default 1e-10)

- verbose:

  logical, print progress messages

## Value

A list with comparison results including:

- identical:

  logical, TRUE if all results match

- new_result:

  result from new chunked method

- legacy_result:

  result from legacy method

- differences:

  list of any differences found
