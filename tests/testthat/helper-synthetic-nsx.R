# Synthetic 'Blackrock' NSx files, so the NSx reader can be tested without real
# recordings. The byte layout follows the 2.3 and 3.0 specifications shipped in
# `inst/specifications`: a 314-byte basic header, one 66-byte `CC` extended
# header per channel, then data blocks. Each data block is a 0x01 byte, a
# timestamp (4 bytes in 2.x, 8 bytes in 3.0), the number of data points, and
# the data points (one int16 per channel each).

# little-endian unsigned integers of `size` bytes; exact below 2^53
nsx_unsigned <- function(x, size) {
  x <- as.numeric(x)
  out <- matrix(0, nrow = size, ncol = length(x))
  for (k in seq_len(size)) {
    out[k, ] <- x %% 256
    x <- x %/% 256
  }
  as.raw(out)
}

nsx_int16 <- function(x) {
  writeBin(as.integer(x), raw(), size = 2, endian = "little")
}

nsx_text <- function(s, n) {
  c(charToRaw(s), raw(n))[seq_len(n)]
}

# The analog range maps one digital count to 0.25 uV, so imported values are
# `values * 0.25`
nsx_uv_per_count <- 0.25

# `values`: integer matrix, channels x data points (raw digital counts)
# `blocks`: list of data blocks in file order; each has `timestamp` and
#   `points` (column indices into `values`), and optionally `declared`, the
#   "Number of Data Points" to write instead of `length(points)`
write_synthetic_nsx <- function(path, values, blocks, version = "3.0",
                                period = 15L, time_resolution = 30000) {
  n_channels <- nrow(values)
  header <- c(
    nsx_text(ifelse(version == "3.0", "BRSMPGRP", "NEURALCD"), 8),
    as.raw(as.integer(strsplit(version, ".", fixed = TRUE)[[1]])),
    nsx_unsigned(314 + 66 * n_channels, 4),
    nsx_text("2 kS/s", 16),
    nsx_text("synthetic data", 256),
    nsx_unsigned(period, 4),
    nsx_unsigned(time_resolution, 4),
    nsx_unsigned(c(2026, 10, 3, 28, 12, 30, 45, 500), 2),
    nsx_unsigned(n_channels, 4)
  )
  extended <- lapply(seq_len(n_channels), function(ii) {
    c(
      nsx_text("CC", 2), nsx_unsigned(ii, 2), nsx_text(sprintf("ch%d", ii), 16),
      as.raw(1L), as.raw(ii),
      nsx_int16(c(-32764, 32764, -8191, 8191)),
      nsx_text("uV", 16),
      nsx_unsigned(c(7500000, 3), 4), nsx_unsigned(1, 2),
      nsx_unsigned(c(300, 1), 4), nsx_unsigned(1, 2)
    )
  })
  timestamp_size <- ifelse(version == "3.0", 8, 4)
  body <- lapply(blocks, function(b) {
    declared <- b$declared
    if (is.null(declared)) { declared <- length(b$points) }
    c(
      as.raw(1L),
      nsx_unsigned(b$timestamp, timestamp_size),
      nsx_unsigned(declared, 4),
      nsx_int16(values[, b$points])
    )
  })
  writeBin(c(header, unlist(extended), unlist(body)), path)
  invisible(path)
}

# one data block per data point, as in recordings with PTP (nanosecond)
# timestamps
nsx_ptp_blocks <- function(timestamps) {
  lapply(seq_along(timestamps), function(ii) {
    list(timestamp = timestamps[[ii]], points = ii)
  })
}

# a deterministic signal: channels x data points
nsx_fixture_values <- function(n_channels = 3, n_points = 20) {
  matrix(
    ((seq_len(n_channels * n_points) * 37L) %% 2001L) - 1000L,
    nrow = n_channels
  )
}

# imported values of one channel, every partition concatenated
nsx_imported_channel <- function(nsx, channel_id) {
  channel <- get_channel(nsx, channel_id)
  unlist(lapply(channel$channel_detail, function(part) part$data[]),
         use.names = FALSE)
}

nsx_partition_meta <- function(nsx, channel_id = 1) {
  channel <- get_channel(nsx, channel_id)
  do.call(rbind, lapply(channel$channel_detail, function(part) {
    data.frame(relative_time = part$meta$relative_time,
               duration = part$meta$duration,
               n_points = length(part$data[]))
  }))
}
