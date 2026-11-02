# Legacy version of read_nsx that reads all data into memory at once
# This version has R's 2^31-1 vector length limit
read_nsx_legacy <- function(path, prefix = NULL, fresh_start = FALSE, spec = NULL, partition_prefix = "/part") {

  # get "x" in NSx
  which_nsx <- tolower(substring(path, nchar(path) - 2))
  if (!which_nsx %in% sprintf("ns%d", seq_len(9))) {
    stop("read_nsx: path must ends with .ns1 to .ns9")
  }

  partition_prefix <- gsub(".", "_", partition_prefix, fixed = TRUE)
  partition_prefix <- gsub("[\\/]{1,}$", "", partition_prefix)

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (missing(spec) || is.null(spec)) {
    ftype <- get_file_type(path = path)
    spec <- get_specification(ftype$version, "nsx")
  } else if (!inherits(spec, "readNSx_specification_nsx")) {
    stop("[readNSx::read_nsx] `spec` must inherit class [readNSx_specification_nsx]. If you don't know what that is, leave this argument blank or NULL")
  }

  file_size <- file.size(path)
  file_name <- basename(path)

  if (!length(prefix)) {
    prefix <- normalizePath(path)
  }

  export_root <- dirname(prefix)
  export_filebase <- basename(prefix)
  dir_create2( export_root )

  conn <- file(path, "rb")
  on.exit({
    tryCatch({
      close(conn)
    }, error = function(...) {})
  }, add = TRUE, after = TRUE)

  # Obtain basic header information
  header_basic <- structure(
    read_sequential(conn, spec$specification$section1$dictionary),
    class = c("readNSx_nsx_basic_header", "readNSx_printable")
  )
  # save basic header
  basic_summary <- data.frame(
    filename = sprintf("%s_ieeg", export_filebase),
    acq_time = format_time_origin( header_basic$time_origin ),
    original_filename = file_name,
    file_type = header_basic$file_type,
    file_spec = paste(header_basic$file_spec, collapse = "."),
    period = header_basic$period,
    channel_count = header_basic$channel_count,
    label = header_basic$label,
    time_resolution_timestamp = header_basic$time_resolution_timestamp
  )

  sample_rate <- 30000 / header_basic$period


  header_extended <- structure(
    read_keyvalue_pairs(
      conn = conn,
      rules = spec$specification$section2$key_rule,
      item_list = spec$specification$section2$dictionary,
      expected_items = header_basic$channel_count
    ),
    class = c("readNSx_nsx_extended_header", "readNSx_printable")
  )

  # save CC
  tbl <- header_extended$CC

  # will convert to uV when importing data
  units <- tbl$units
  units[units %in% names(physical_unit_lut)] <- "uV"
  channel_table <- data.frame(
    name = tbl$electrode_label,
    type = tbl$type,
    units = units,
    sampling_frequency = sample_rate,
    low_freq_type = tbl$low_freq_type,
    low_freq_order = tbl$low_freq_order,
    low_freq_corner = tbl$low_freq_corner,
    high_freq_type = tbl$high_freq_type,
    high_freq_order = tbl$high_freq_order,
    high_freq_corner = tbl$high_freq_corner,
    original_channel = tbl$electrode_id,
    original_filename = file_name
  )
  append_table_rds(
    path = file.path(export_root, sprintf("%s_channels.rds", export_filebase)),
    new_tbl = channel_table,
    index_column = "original_filename", obsolete_values = file_name)

  nsx_data <- structure(
    list(
      header_basic = header_basic,
      header_extended = header_extended,
      nparts = 0,
      which = which_nsx,
      prefix = prefix,
      partition_prefix = partition_prefix
    ),
    class = c("readNSx_nsx", "readNSx_printable")
  )

  # let's get remainly file size in bytes
  data_bytes <- file_size - header_basic$bytes_in_headers

  # preparation
  n_channels <- header_basic$channel_count

  # Calculate digital to analog transformation
  units <- sapply(header_extended$CC$units[seq_len(n_channels)], function(unit) {
    rate <- physical_unit_lut[[unit]]
    if (length(rate) != 1) { rate <- 1 }
    rate
  })
  min_digit <- header_extended$CC$min_digital_value[seq_len(n_channels)]
  max_digit <- header_extended$CC$max_digital_value[seq_len(n_channels)]
  min_analog <- header_extended$CC$min_analog_value[seq_len(n_channels)] * units
  max_analog <- header_extended$CC$max_analog_value[seq_len(n_channels)] * units
  slope <- (max_analog - min_analog) / (max_digit - min_digit)
  intercept <- ((max_analog + min_analog) - slope * (max_digit + min_digit)) / 2

  current_partition <- 1
  start_time <- 0

  # Read data (all at once - limited by R's vector length 2^31-1)
  if (header_basic$file_spec[[1]] >= 3) {
    nsx_signal_data <- readNSxDataPacket30(
      filePath = path, nBytes = data_bytes, sampleRate = sample_rate,
      nChannels = n_channels, skipBytes = header_basic$bytes_in_headers,
      slope = 1.0, intercept = 0.0
    )
  } else {
    nsx_signal_data <- readNSxDataPacket2x(
      filePath = path, nBytes = data_bytes, sampleRate = sample_rate,
      nChannels = n_channels, skipBytes = header_basic$bytes_in_headers,
      slope = 1.0, intercept = 0.0
    )
  }
  nsx_signal_data$data <- matrix(nsx_signal_data$data, nrow = n_channels, byrow = FALSE) * slope + intercept

  timestamps <- nsx_signal_data$timestamps
  timestamps <- round((timestamps - timestamps[[1]]) * sample_rate)
  idx <- deparse_svec(timestamps, concatenate = FALSE, max_lag = 2, connect = ",")

  nsx_data$nparts <- length(idx)

  start_time <- min(nsx_signal_data$timestamps)

  lapply(seq_along(idx), function(current_partition) {

    idx_range <- eval(parse(text = sprintf("c(%s)", idx[[current_partition]])))
    sel <- timestamps >= idx_range[[1]] & timestamps <= idx_range[[2]]
    time_range <- range(nsx_signal_data$timestamps[sel])

    filename <- sprintf("%s_ieeg%s%d", export_filebase, partition_prefix, current_partition)

    if ( header_basic$file_spec[[1]] >= 3 ) {
      start_time <- time_range[[1]]
    }

    tbl <- data.frame(
      filename = filename,
      original_filename = file_name,
      partition = current_partition,
      duration = time_range[[2]] - time_range[[1]],
      relative_time = start_time,
      sample_rate = sample_rate,
      internal_partition_key = sprintf("%s__part%s", file_name, current_partition)
    )
    dir <- file.path(export_root, filename)
    if (fresh_start && dir.exists(dir)) {
      unlink(dir, recursive = TRUE, force = TRUE)
    }
    dir_create2(dir)
    append_table_rds(
      path = file.path(dirname(dir), "partition_info.rds"), new_tbl = tbl,
      index_column = "internal_partition_key", obsolete_values = tbl$internal_partition_key)

    label_contains_channel <- endsWith(
      header_extended$CC$electrode_label[seq_len(n_channels)],
      as.character(header_extended$CC$electrode_id)[seq_len(n_channels)])

    lapply(seq_len(n_channels), function(ii) {
      channel_id <- header_extended$CC$electrode_id[[ii]]
      channel_label <- header_extended$CC$electrode_label[[ii]]
      fname <- file.path(dir, channel_filename(
        channel_id = channel_id,
        channel_label = channel_label))

      # save meta information
      save_h5(
        x = jsonlite::toJSON(as.list(tbl), auto_unbox = TRUE),
        file = fname, name = "meta", ctype = "character",
        replace = TRUE, new_file = FALSE, quiet = TRUE
      )
      save_h5(
        x = nsx_signal_data$data[ii, sel],
        file = fname, name = "data", ctype = "numeric",
        replace = TRUE, new_file = FALSE, quiet = TRUE, chunk = 16384L
      )
      return()
    })

  })

  ieeg_path <- file.path(export_root, sprintf("%s_ieeg", export_filebase))
  dir_create2(ieeg_path)
  # save partition summary
  basic_summary$n_partitions <- nsx_data$nparts
  append_table_rds(
    path = file.path(ieeg_path, "configurations.rds"),
    new_tbl = basic_summary,
    index_column = "original_filename", obsolete_values = file_name)

  # save nsx_data
  saveRDS(nsx_data, file = file.path(ieeg_path, sprintf("%s_summary.rds", which_nsx)))

  return(nsx_data)
}


# Options that control how `read_nsx` reads data points, checked before
# anything is written so that a bad value fails early and names the option
nsx_reader_options <- function() {
  is_number <- function(x) {
    is.numeric(x) && length(x) == 1 && isTRUE(is.finite(x))
  }
  chunk_size <- getOption("readNSx.chunk_size", 1000000L)
  if (!is_number(chunk_size) || chunk_size < 1) {
    stop("Option 'readNSx.chunk_size' (data points read per batch) must be a number of at least 1")
  }
  packets_per_frame <- getOption("readNSx.packets_per_frame", 100000)
  if (!is_number(packets_per_frame) || packets_per_frame < 2 ||
      packets_per_frame != floor(packets_per_frame)) {
    stop("Option 'readNSx.packets_per_frame' must be a whole number of at least 2")
  }
  max_tick_multiple <- getOption("readNSx.max_tick_multiple", 2)
  if (!is_number(max_tick_multiple) || max_tick_multiple <= 1) {
    stop("Option 'readNSx.max_tick_multiple' must be a number greater than 1")
  }
  list(
    chunk_size = floor(as.numeric(chunk_size)),
    packets_per_frame = as.numeric(packets_per_frame),
    max_tick_multiple = as.numeric(max_tick_multiple)
  )
}

# internally used. Do not call by yourself
read_nsx <- function(path, prefix = NULL, fresh_start = FALSE, spec = NULL, partition_prefix = "/part") {

  # get "x" in NSx
  which_nsx <- tolower(substring(path, nchar(path) - 2))
  if (!which_nsx %in% sprintf("ns%d", seq_len(9))) {
    stop("read_nsx: path must ends with .ns1 to .ns9")
  }

  partition_prefix <- gsub("[.]", "_", partition_prefix)
  partition_prefix <- gsub("[\\/]{1,}$", "", partition_prefix)

  reader_options <- nsx_reader_options()

  # DIPSAUS DEBUG START
  # path <- "/Users/dipterix/rave_data/raw_dir/POR006/b01/datafile_05_09_25.ns5"
  # prefix <- '/Users/dipterix/rave_data/raw_dir/POR006/b01/datafile_05_09_25'
  # ftype <- get_file_type(path = path)
  # spec <- get_specification(ftype$version, "nsx")
  # fresh_start <- FALSE
  # which_nsx = tolower(substring(path, nchar(path) - 2))
  # partition_prefix = "/part"

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (missing(spec) || is.null(spec)) {
    ftype <- get_file_type(path = path)
    spec <- get_specification(ftype$version, "nsx")
  } else if (!inherits(spec, "readNSx_specification_nsx")) {
    stop("[readNSx::read_nsx] `spec` must inherit class [readNSx_specification_nsx]. If you don't know what that is, leave this argument blank or NULL")
  }

  file_size <- file.size(path)
  file_name <- basename(path)

  if (!length(prefix)) {
    prefix <- normalizePath(path)
  }

  export_root <- dirname(prefix)
  export_filebase <- basename(prefix)
  dir_create2( export_root )

  # exclude_packets <- parse_svec(exclude_packets)
  conn <- file(path, "rb")
  on.exit({
    tryCatch({
      close(conn)
    }, error = function(...) {})
  }, add = TRUE, after = TRUE)

  # Obtain basic header information
  header_basic <- structure(
    read_sequential(conn, spec$specification$section1$dictionary),
    class = c("readNSx_nsx_basic_header", "readNSx_printable")
  )

  # Timestamps count ticks of a clock running at `time_resolution_timestamp`
  # Hz: 30000 in most files, 1e9 (nanoseconds) with PTP timestamps. Some
  # acquisition software writes an implausible value here; fall back to the
  # 30 kHz clock. `read_nev` has a similar guard, but it only applies to spec
  # 3.0 and replaces any value below 30000
  if (!isTRUE(header_basic$time_resolution_timestamp > 2000)) {
    header_basic$time_resolution_timestamp <- 30000
  }

  # save basic header
  basic_summary <- data.frame(
    filename = sprintf("%s_ieeg", export_filebase),
    acq_time = format_time_origin( header_basic$time_origin ),
    original_filename = file_name,
    file_type = header_basic$file_type,
    file_spec = paste(header_basic$file_spec, collapse = "."),
    period = header_basic$period,
    channel_count = header_basic$channel_count,
    label = header_basic$label,
    time_resolution_timestamp = header_basic$time_resolution_timestamp
  )

  sample_rate <- 30000 / header_basic$period


  header_extended <- structure(
    read_keyvalue_pairs(
      conn = conn,
      rules = spec$specification$section2$key_rule,
      item_list = spec$specification$section2$dictionary,
      expected_items = header_basic$channel_count
    ),
    class = c("readNSx_nsx_extended_header", "readNSx_printable")
  )

  # save CC
  tbl <- header_extended$CC

  # will convert to uV when importing data
  units <- tbl$units
  units[units %in% names(physical_unit_lut)] <- "uV"
  channel_table <- data.frame(
    name = tbl$electrode_label,
    type = tbl$type,
    units = units,
    sampling_frequency = sample_rate,
    low_freq_type = tbl$low_freq_type,
    low_freq_order = tbl$low_freq_order,
    low_freq_corner = tbl$low_freq_corner,
    high_freq_type = tbl$high_freq_type,
    high_freq_order = tbl$high_freq_order,
    high_freq_corner = tbl$high_freq_corner,
    original_channel = tbl$electrode_id,
    original_filename = file_name
  )
  append_table_rds(
    path = file.path(export_root, sprintf("%s_channels.rds", export_filebase)),
    new_tbl = channel_table,
    index_column = "original_filename", obsolete_values = file_name)

  nsx_data <- structure(
    list(
      header_basic = header_basic,
      header_extended = header_extended,
      nparts = 0,
      which = which_nsx,
      prefix = prefix,
      partition_prefix = partition_prefix
    ),
    class = c("readNSx_nsx", "readNSx_printable")
  )

  # let's get remainly file size in bytes
  data_bytes <- file_size - header_basic$bytes_in_headers

  # preparation
  n_channels <- header_basic$channel_count

  # Calculate digital to analog transformation
  units <- sapply(header_extended$CC$units[seq_len(n_channels)], function(unit) {
    rate <- physical_unit_lut[[unit]]
    if (length(rate) != 1) { rate <- 1 }
    rate
  })
  min_digit <- header_extended$CC$min_digital_value[seq_len(n_channels)]
  max_digit <- header_extended$CC$max_digital_value[seq_len(n_channels)]

  min_analog <- header_extended$CC$min_analog_value[seq_len(n_channels)] * units
  max_analog <- header_extended$CC$max_analog_value[seq_len(n_channels)] * units
  slope <- (max_analog - min_analog) / (max_digit - min_digit)
  intercept <- ((max_analog + min_analog) - slope * (max_digit + min_digit)) / 2

  # =============================================================================
  # Step 1: Scan the data blocks into partitions (no data values are read)
  # =============================================================================
  # A partition is one continuous run of data points: a data block holding many
  # data points, or a run of one-point data blocks (PTP timestamps) that is
  # split only at pauses. See `scanNSxPartitions` in src/readNSxDataPacket.cpp
  partitions <- scanNSxPartitions(
    filePath = path,
    skipBytes = header_basic$bytes_in_headers,
    nBytes = data_bytes,
    nChannels = n_channels,
    timestampBytes = if (header_basic$file_spec[[1]] >= 3) 8L else 4L,
    timestampResolution = header_basic$time_resolution_timestamp,
    sampleRate = sample_rate,
    packetsPerFrame = reader_options$packets_per_frame,
    maxTickMultiple = reader_options$max_tick_multiple
  )

  nsx_data$nparts <- length(partitions$n_points)

  if (nzchar(partitions$stop_reason)) {
    warning(sprintf(
      "NSx file %s: stopped reading because %s; %.0f bytes at the end of the file were not imported",
      file_name, partitions$stop_reason, partitions$bytes_unread
    ), call. = FALSE)
  }
  if (nsx_data$nparts == 0) {
    warning(sprintf("NSx file %s contains no data points", file_name), call. = FALSE)
  }

  # =============================================================================
  # Step 2: Write each partition in batches of data points
  # =============================================================================
  # The batch size bounds memory use (channels x batch values). Counts and
  # positions stay doubles, so partitions longer than 2^31 data points work
  chunk_size <- reader_options$chunk_size

  lapply(seq_along(partitions$n_points), function(current_partition) {

    n_points <- partitions$n_points[[current_partition]]

    filename <- sprintf("%s_ieeg%s%d", export_filebase, partition_prefix, current_partition)

    tbl <- data.frame(
      filename = filename,
      original_filename = file_name,
      partition = current_partition,
      duration = n_points / sample_rate,
      relative_time = partitions$start_time[[current_partition]],
      sample_rate = sample_rate,
      internal_partition_key = sprintf("%s__part%s", file_name, current_partition)
    )
    dir <- file.path(export_root, filename)
    if (fresh_start && dir.exists(dir)) {
      unlink(dir, recursive = TRUE, force = TRUE)
    }
    dir_create2(dir)
    append_table_rds(
      path = file.path(dirname(dir), "partition_info.rds"), new_tbl = tbl,
      index_column = "internal_partition_key", obsolete_values = tbl$internal_partition_key)

    # Pre-allocate HDF5 files for each channel
    h5_files <- lapply(seq_len(n_channels), function(ii) {
      channel_id <- header_extended$CC$electrode_id[[ii]]
      channel_label <- header_extended$CC$electrode_label[[ii]]
      fname <- file.path(dir, channel_filename(
        channel_id = channel_id,
        channel_label = channel_label))

      # save meta information
      # 17 significant digits: `relative_time` must survive the JSON round
      # trip exactly (the default keeps 4 decimals, up to 50 us off)
      save_h5(
        x = jsonlite::toJSON(as.list(tbl), auto_unbox = TRUE, digits = I(17)),
        file = fname, name = "meta", ctype = "character",
        replace = TRUE, new_file = FALSE, quiet = TRUE
      )

      # Pre-allocate data array
      allocate_h5(
        file = fname, name = "data",
        dims = n_points,
        chunk = 16384L, level = 4,
        replace = TRUE, new_file = FALSE, ctype = "numeric", quiet = TRUE
      )

      return(fname)
    })

    first_point <- 0
    while (first_point < n_points) {
      n_batch <- min(chunk_size, n_points - first_point)

      # n_channels x n_batch matrix
      batch <- readNSxPoints(
        filePath = path,
        byteOffset = partitions$byte_offset[[current_partition]],
        stride = partitions$stride[[current_partition]],
        nChannels = n_channels,
        slope = slope,
        intercept = intercept,
        firstPoint = first_point,
        nPoints = n_batch
      )

      for (ii in seq_len(n_channels)) {
        write_h5_slice(
          x = batch[ii, ],
          file = h5_files[[ii]],
          name = "data",
          start = first_point + 1,
          quiet = TRUE
        )
      }

      first_point <- first_point + n_batch
    }

    return()
  })

  # while( data_bytes > 0 ) {
  #   packet_header <- read_sequential(conn, packet_header_spec)
  #   packet_header <- packet_header[[1]]
  #   data_bytes <- data_bytes - header_item$.bytes
  #
  #   if ( !isTRUE(packet_header$header == 1) ) {
  #     warning("NSx has invalid data header: the header must starts with 0x01. The data might be incomplete.")
  #     break
  #   }
  #
  #   n_sample_points <- packet_header$number_of_data_points
  #   if ( n_sample_points <= 0 ) {
  #     warning("NSx: detected zero-length signals; skipping.")
  #     next
  #   }
  #
  #   item$n <- n_sample_points * n_channels
  #   item <- do.call(validate_spec, item)
  #
  #   if ( item$.bytes > data_bytes ) {
  #     warning("NSx: requested data length is greater than the file size. The data might be incomplete.")
  #     break
  #   }
  #
  #   buf <- readBin(conn, what = "raw", n = item$.bytes, size = 1L, endian = "little")
  #   data_bytes <- data_bytes - item$.bytes
  #   buf <- parse_value(buf)
  #   dim(buf) <- c(n_channels, n_sample_points)
  #   buf <- buf * slope + intercept
  #
  #   # do not use sample rate, use `time_resolution_timestamp` (30000)
  #   start_time <- start_time + packet_header$timestamp / header_basic$time_resolution_timestamp
  #   duration <- n_sample_points / sample_rate
  #
  #   # write to disk
  #   filename <- sprintf("%s_ieeg%s%d", export_filebase, partition_prefix, current_partition)
  #   dir <- file.path(export_root, filename)
  #   if (fresh_start && dir.exists(dir)) {
  #     unlink(dir, recursive = TRUE, force = TRUE)
  #   }
  #   dir_create2(dir)
  #
  #   tbl <- data.frame(
  #     filename = filename,
  #     original_filename = file_name,
  #     partition = current_partition,
  #     duration = duration,
  #     relative_time = start_time,
  #     sample_rate = sample_rate,
  #     internal_partition_key = sprintf("%s__part%s", file_name, current_partition)
  #   )
  #   append_table_rds(
  #     path = file.path(dir, "..", "partition_info.rds"), new_tbl = tbl,
  #     index_column = "internal_partition_key", obsolete_values = tbl$internal_partition_key)
  #
  #   label_contains_channel <- endsWith(
  #     header_extended$CC$electrode_label[seq_len(n_channels)],
  #     as.character(header_extended$CC$electrode_id)[seq_len(n_channels)])
  #
  #   lapply(seq_len(n_channels), function(ii) {
  #     channel_id <- header_extended$CC$electrode_id[[ii]]
  #     channel_label <- header_extended$CC$electrode_label[[ii]]
  #     fname <- file.path(dir, channel_filename(
  #       channel_id = channel_id,
  #       channel_label = channel_label))
  #
  #     # save meta information
  #     save_h5(
  #       x = jsonlite::toJSON(as.list(tbl), auto_unbox = TRUE),
  #       file = fname, name = "meta", ctype = "character",
  #       replace = TRUE, new_file = FALSE, quiet = TRUE
  #     )
  #     save_h5(
  #       x = buf[ii,],
  #       file = fname, name = "data", ctype = "numeric",
  #       replace = TRUE, new_file = FALSE, quiet = TRUE, chunk = 16384L
  #     )
  #     return()
  #   })
  #
  #   start_time <- start_time + duration
  #   nsx_data$nparts <- current_partition
  #   current_partition <- current_partition + 1
  # }

  ieeg_path <- file.path(export_root, sprintf("%s_ieeg", export_filebase))
  dir_create2(ieeg_path)
  # save partition summary
  basic_summary$n_partitions <- nsx_data$nparts
  append_table_rds(
    path = file.path(ieeg_path, "configurations.rds"),
    new_tbl = basic_summary,
    index_column = "original_filename", obsolete_values = file_name)

  # save nsx_data
  saveRDS(nsx_data, file = file.path(ieeg_path, sprintf("%s_summary.rds", which_nsx)))

  return(nsx_data)
}


