# readNSx 0.0.5

* Fixed calling `R_MakeExternalPtr` with `Rf_mkChar`, using `Rf_install` instead
* Fixed a potential imbalanced protect

# readNSx 0.0.4

* Fixed a `ASAN/UBSAN` error in `read_bci2000`

# readNSx 0.0.3

* Added `read_bci2000` to read `BCI2000` data

# readNSx 0.0.2

* Added support: `NEV/NSx` 3.0 specification
* Corrected `NEV` 3.0 file bug. In some cases, the acquisition software may incorrectly set "Time Resolution Time-stamp", resulting in wrong event time-stamps

# readNSx 0.0.1

* Added a `NEWS.md` file to track changes to the package.
