# Synthetic 'Blackrock' NEV files, so the NEV reader can be tested without real
# recordings. The byte layout follows the 2.3 and 3.0 specifications shipped in
# `inst/specifications` (basic header, 32-byte extended headers, fixed-size
# data packets).

nev_bytes <- function(x, size) {
  x <- as.numeric(x)
  if (size == 4) {
    # unsigned 32-bit values above 2^31 - 1 are written as their two's
    # complement counterpart
    x <- ifelse(x >= 2^31, x - 2^32, x)
  }
  writeBin(as.integer(x), raw(), size = size, endian = "little")
}

nev_text <- function(s, n) {
  c(charToRaw(s), raw(n))[seq_len(n)]
}

nev_timestamp <- function(timestamp, version) {
  if (version == "3.0") {
    c(nev_bytes(timestamp %% 2^32, 4), nev_bytes(timestamp %/% 2^32, 4))
  } else {
    nev_bytes(timestamp, 4)
  }
}

# `electrodes`: data frame with `electrode_id`, `label`, `digitization_factor`,
#   and `bytes_per_waveform` (bytes per waveform sample)
# `packets`: list of packets in file order; each has `type` ("spike",
#   "digital_inputs", or "comment"), `timestamp`, and
#   - spike: `packet_id` (electrode), `unit`, `samples` (raw integer counts)
#   - digital_inputs: `reason` (insertion reason byte), `value`
#   - comment: `data`, `text`
write_synthetic_nev <- function(path, electrodes, packets, version = "2.3",
                                bytes_in_data_packet = 104L,
                                additional_flags = 1L,
                                time_resolution = 30000L) {
  n_extended <- 2L * nrow(electrodes)
  header <- c(
    nev_text(ifelse(version == "3.0", "BREVENTS", "NEURALEV"), 8),
    as.raw(as.integer(strsplit(version, ".", fixed = TRUE)[[1]])),
    nev_bytes(additional_flags, 2),
    nev_bytes(336L + 32L * n_extended, 4),
    nev_bytes(bytes_in_data_packet, 4),
    nev_bytes(time_resolution, 4),
    nev_bytes(time_resolution, 4),
    nev_bytes(c(2026, 10, 4, 15, 12, 30, 45, 500), 2),
    nev_text("readNSx tests", 32),
    nev_text("synthetic data", 256),
    nev_bytes(n_extended, 4)
  )
  extended <- lapply(seq_len(nrow(electrodes)), function(ii) {
    e <- electrodes[ii, ]
    c(
      nev_text("NEUEVWAV", 8), nev_bytes(e$electrode_id, 2),
      as.raw(1L), as.raw(ii), nev_bytes(e$digitization_factor, 2),
      nev_bytes(0L, 2), nev_bytes(800L, 2), nev_bytes(-800L, 2),
      as.raw(0L), as.raw(e$bytes_per_waveform), nev_bytes(48L, 2), raw(8),
      nev_text("NEUEVLBL", 8), nev_bytes(e$electrode_id, 2),
      nev_text(e$label, 16), raw(6)
    )
  })
  body <- lapply(packets, function(p) {
    head <- nev_timestamp(p$timestamp, version)
    switch(
      p$type,
      "spike" = {
        sample_size <- electrodes$bytes_per_waveform[
          electrodes$electrode_id == p$packet_id]
        if (additional_flags == 1L) { sample_size <- 2L }
        head <- c(head, nev_bytes(p$packet_id, 2), as.raw(p$unit), as.raw(0L))
        rest <- nev_bytes(p$samples, sample_size)
      },
      "digital_inputs" = {
        head <- c(head, nev_bytes(0L, 2), as.raw(p$reason), as.raw(0L))
        rest <- nev_bytes(p$value, 2)
      },
      "comment" = {
        head <- c(head, nev_bytes(65535L, 2), as.raw(0L), as.raw(0L))
        rest <- c(nev_bytes(p$data, 4), charToRaw(p$text))
      }
    )
    packet <- c(head, rest)
    stopifnot(length(packet) <= bytes_in_data_packet)
    c(packet, raw(bytes_in_data_packet - length(packet)))
  })
  writeBin(c(header, unlist(extended), unlist(body)), path)
  invisible(path)
}

# A small recording with digital inputs, a comment, and spikes on electrodes
# with 1- and 2-byte waveform samples. Timestamps cross 2^31 (unsigned 32-bit)
# and, for version 3.0, 2^32 (the high word of the 64-bit timestamp).
synthetic_nev_fixture <- function(version = "2.3", additional_flags = 0L) {
  electrodes <- data.frame(
    electrode_id = c(1L, 2L, 3L),
    label = c("LA1", "LA2", "RB1"),
    digitization_factor = c(250L, 1000L, 125L),
    bytes_per_waveform = c(2L, 1L, 2L)
  )
  # waveform bytes: 104 - 8 (2.x) or 104 - 12 (3.0)
  wave_bytes <- ifelse(version == "3.0", 92L, 96L)
  n_2byte <- wave_bytes %/% 2L
  n_1byte <- ifelse(additional_flags == 1L, n_2byte, wave_bytes)
  last_timestamp <- ifelse(version == "3.0", 2^32 + 7, 4e9)
  packets <- list(
    list(type = "digital_inputs", timestamp = 100, reason = 1L, value = 5L),
    list(type = "spike", timestamp = 200, packet_id = 1L, unit = 1L,
         samples = (seq_len(n_2byte) - 25L) * 10L),
    list(type = "spike", timestamp = 300, packet_id = 2L, unit = 0L,
         samples = rep(c(-3L, 7L), length.out = n_1byte)),
    list(type = "comment", timestamp = 400, data = 42L, text = "trial start"),
    list(type = "spike", timestamp = 3e9, packet_id = 3L, unit = 2L,
         samples = seq_len(n_2byte) * 100L),
    list(type = "digital_inputs", timestamp = 3000000100, reason = 1L,
         value = 65535L),
    list(type = "spike", timestamp = last_timestamp, packet_id = 1L, unit = 0L,
         samples = rep(32767L, n_2byte))
  )
  list(electrodes = electrodes, packets = packets, version = version,
       additional_flags = additional_flags)
}

write_nev_fixture <- function(fixture, path) {
  write_synthetic_nev(
    path, electrodes = fixture$electrodes, packets = fixture$packets,
    version = fixture$version, additional_flags = fixture$additional_flags
  )
}
