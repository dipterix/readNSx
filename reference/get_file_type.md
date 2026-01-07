# Get 'Blackrock' file type

Reads the first 10 bytes containing file type and version information.

## Usage

``` r
get_file_type(path)
```

## Arguments

- path:

  path to the 'Blackrock' `'.nev'` or `'.nsx'` file, or a binary
  connection.

## Value

A list containing file information, including file type, version
information, and normalized absolute path.
