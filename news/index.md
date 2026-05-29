# Changelog

## readNSx 0.0.7

CRAN release: 2026-05-23

- `import_nsp` Supports streaming the data with lengths larger than the
  bound of 32-bit integers. This supports high-frequency long-time
  continuous recording, extending the `30,000 Hz` recording duration
  from 10 minutes (100 channels) to Almost a half day recording. The
  individual channel size is still limited by the 32-bit integer, that
  is around 19 hours
- Fixed an `rchk` issue reported by `CRAN`
- Replaced `c.integer64` with `c`

## readNSx 0.0.6

CRAN release: 2025-10-24

- `HDF5` falls back to alternatives in `WASM` to avoid compilation
  issues when running in web browser

## readNSx 0.0.5

CRAN release: 2024-09-03

- Fixed calling `R_MakeExternalPtr` with `Rf_mkChar`, using `Rf_install`
  instead
- Fixed a potential imbalanced protect

## readNSx 0.0.4

CRAN release: 2024-02-24

- Fixed a `ASAN/UBSAN` error in `read_bci2000`

## readNSx 0.0.3

CRAN release: 2024-02-17

- Added `read_bci2000` to read `BCI2000` data

## readNSx 0.0.2

CRAN release: 2023-06-21

- Added support: `NEV/NSx` 3.0 specification
- Corrected `NEV` 3.0 file bug. In some cases, the acquisition software
  may incorrectly set “Time Resolution Time-stamp”, resulting in wrong
  event time-stamps

## readNSx 0.0.1

CRAN release: 2023-02-01

- Added a `NEWS.md` file to track changes to the package.
