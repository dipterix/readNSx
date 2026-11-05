# Changelog

## readNSx 0.2.0

- `NEV` data packets are now read in chunks, and the packet types of a
  whole chunk are checked at once. Excluded events (by default `spike`)
  no longer cost any parsing, and memory use no longer grows with the
  file size, so importing a `NEV` file that stores the waveform of every
  spike is many times faster
- The waveform of each spike is parsed for a whole chunk at once and
  written to `waveforms.h5` while reading
- Fixed the spike waveform being cut to its first 4 (`NEV` 2.2 and 2.3)
  or 6 (`NEV` 3.0) samples. The `waveform` data set in `waveforms.h5` is
  now a matrix with all samples of each spike, one column per spike
  (zero-padded when electrodes store samples in different sizes); it
  used to be a vector
- Fixed an error when importing spikes from a recording that has no
  spikes
- Added
  [`read_nev_header()`](https://dipterix.org/readNSx/reference/read_nev_header.md)
  to read the `NEV` headers, such as the channel labels, without reading
  the data packets
- Importing `NSx` files that store one data point per data block
  (recordings with precision time protocol, `PTP`, time-stamps) no
  longer takes days: data points are read and written to `HDF5` in
  batches instead of once per data block and channel. Partitions are
  found while scanning the data block headers, so memory use no longer
  grows with the length of the recording
- In those files, clock jumps that cancel out within a frame of
  `readNSx.packets_per_frame` data blocks, and backward jumps, no longer
  split a continuous recording into several partitions. As in `NPMK`
  `openNSx`, a forward jump of more than `readNSx.max_tick_multiple`
  sampling intervals still starts a new partition. In other `NSx` files,
  each data block is one partition: unchanged for files with a `30 kHz`
  time-stamp clock, while files with nanosecond time-stamps used to
  merge data blocks whose time-stamps lined up. If such a file’s
  time-stamps do not tick at the header’s `time_resolution_timestamp`
  (more than half of its data points would start a new partition), the
  import stops with an error naming that field instead of writing one
  partition per data point
- `NSx` time-stamps are converted to seconds with the file’s
  `time_resolution_timestamp` instead of always as nanoseconds. For
  files with a `30 kHz` time-stamp clock, the `relative_time` of each
  partition used to be 33,333 times too small and now agrees with the
  `NEV` event times. A resolution of 2000 or less is treated as 30000.
  The `relative_time` stored with each channel keeps full precision; it
  used to be rounded to 4 decimals (up to 1.5 samples at `30 kHz`).
  Imports made by earlier versions keep the old values until they are
  imported again
- A data block whose header declares more data points than the file
  holds is read up to its last complete data point instead of failing. A
  warning names the file when it ends early, has a corrupt data block
  header, or holds no data points. A partition can now hold more than
  2^31 - 1 data points
- Invalid values of the options `readNSx.chunk_size`,
  `readNSx.packets_per_frame` and `readNSx.max_tick_multiple` give an
  error naming the option before anything is written;
  `readNSx.chunk_size = 0` used to loop forever
- Fixed results of the `C++` readers (`NSx` scanning, the legacy `NSx`
  reader, and `BCI2000` parameters) being exposed to garbage collection
  before they were returned, which could give wrong values, hangs, or
  crashes
- Writing `HDF5` data to an existing file that is not `HDF5` now gives
  an error and leaves the file alone. The file used to be replaced with
  an empty `HDF5` file without warning, even when only checking whether
  it could be written. An empty file is still replaced, so importing
  again repairs one left behind by an interrupted import
- Fixed the `WebAssembly` build, which has failed since `HDF5` support
  moved to `hdf5lib`, leaving `webR` users on version 0.0.7, which
  cannot read `HDF5` data there. `WebAssembly` builds now link the
  `HDF5` library of the `webR` build tools

## readNSx 0.1.0

CRAN release: 2026-09-02

- `HDF5` is now read and written directly by `readNSx`, linking against
  `hdf5lib` for a static build of the `HDF5` library. This removes the
  dependency on `hdf5r` and the `HDF5` system requirement; no system
  `HDF5` installation is needed on any platform
- Removed the `hdf5r` code path and the plain-`R` `.ralt` fallback
  store; there is now a single storage implementation. The layout on
  disk is unchanged, so files exported by earlier versions read back
  unchanged and files written now stay readable by `hdf5r`, `h5py`, and
  command-line `HDF5` tools
- Channel datasets are no longer capped at `2^31` samples: dimensions
  and offsets are carried as doubles all the way into `HDF5`
- Reading a contiguous range from a channel now reads only that range
  instead of the whole dataset
- `h5_valid(file, "w")` no longer truncates the file it is asked about
- Added complex support, stored as the `H5T_COMPOUND {"r", "i"}` of
  doubles that `numpy` and `h5py` use for `complex128`. Complex datasets
  written elsewhere are read back as complex whatever their member
  names, including the `{"Real", "Imaginary"}` layout `hdf5r` uses and
  `HDF5` 2.0 native complex
- Strings are now written as `UTF-8` rather than being labeled `ASCII`
  regardless of content, so non-`ASCII` text survives. For plain `ASCII`
  the stored bytes are unchanged. Strings are read using whatever
  character set the file declares, so files written by earlier versions
  are unaffected
- `NA_character_` is now stored as an `HDF5` `NULL` and read back as
  `NA`. Previously it became the literal text `"NA"`, which no reader
  could tell apart from a genuine `"NA"` string
- `NA` in integer, double and logical vectors round-trips exactly,
  matching what `hdf5r` stores bit for bit

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
