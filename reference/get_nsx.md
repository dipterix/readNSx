# Load 'NSx' information from path prefix

Load 'NSx' information from path prefix

## Usage

``` r
get_nsx(x, which, ...)
```

## Arguments

- x:

  path `prefix` specified in
  [`import_nsp`](http://dipterix.org/readNSx/reference/import_nsp.md),
  or `'nev/nsx'` object

- which:

  which 'NSx' to load, for example, `which=3` loads `'ns3'` headers

- ...:

  reserved for future use

## Value

'NSx' header information if data is found, otherwise returns `NULL`. See
Section "'NSx' Data" in
[`get_specification`](http://dipterix.org/readNSx/reference/get_specification.md)
