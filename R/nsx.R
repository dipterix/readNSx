
# internally used. Do not call by yourself
read_nsx <- function(path, prefix = NULL, fresh_start = FALSE, spec = NULL, partition_prefix = "/part") {

  # get "x" in NSx
  which_nsx <- tolower(substring(path, nchar(path) - 2))
  if(!which_nsx %in% sprintf("ns%d", seq_len(9))) {
    stop("read_nsx: path must ends with .ns1 to .ns9")
  }

  partition_prefix <- gsub("[.]", "_", partition_prefix)
  partition_prefix <- gsub("[\\/]{1,}$", "", partition_prefix)

  # DIPSAUS DEBUG START
  # path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YEK/EMU-057_subj-YEK_task-LOCALIZER_run-01_NSP-1.ns3'
  # prefix <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/bids/TestData/sub-YEK/ses-057/ieeg/sub-YEK_ses-057_task-localizer_acq-NSP1_run-01'
  # path = "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV032b/BLOCK011/NSP-PAV032b_Datafile_010.ns3"
  # prefix <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV032b/BLOCK011/NSP-PAV032b_Datafile_010_junk"
  # ftype <- get_file_type(path = path)
  # spec <- get_specification(ftype$version, "nsx")
  # fresh_start <- FALSE
  # which_nsx = tolower(substring(path, nchar(path) - 2))
  # partition_prefix = "/part"

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if(missing(spec) || is.null(spec)) {
    ftype <- get_file_type(path = path)
    spec <- get_specification(ftype$version, "nsx")
  } else if(!inherits(spec, "readNSx_specification_nsx")) {
    stop("[readNSx::read_nsx] `spec` must inherit class [readNSx_specification_nsx]. If you don't know what that is, leave this argument blank or NULL")
  }

  file_size <- file.size(path)
  file_name <- basename(path)

  if(!length(prefix)) {
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
    }, error = function(...){})
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
  packet_header_spec <- spec$specification$section3_part1$dictionary
  packet_value_spec <- spec$specification$section3_part2$dictionary
  n_channels <- header_basic$channel_count
  header_item <- packet_header_spec$data_header
  header_item$name <- "data_header"
  header_item <- do.call(validate_spec, header_item)
  item <- packet_value_spec$data_points
  item$name <- "data_points"
  parse_value <- get_parse_function(item$type)

  # Calculate digital to analog transformation
  units <- sapply(header_extended$CC$units[seq_len(n_channels)], function(unit) {
    rate <- physical_unit_lut[[unit]]
    if(length(rate) != 1) { rate <- 1 }
    rate
  })
  min_digit <- header_extended$CC$min_digital_value[seq_len(n_channels)]
  max_digit <- header_extended$CC$max_digital_value[seq_len(n_channels)]
  min_analog <- header_extended$CC$min_analog_value[seq_len(n_channels)] * units
  max_analog <- header_extended$CC$max_analog_value[seq_len(n_channels)] * units
  slope <- (max_analog - min_analog) / (max_digit - min_digit)
  intercept <- ((max_analog + min_analog) - slope * (max_digit + min_digit)) / 2

  current_partition <- 1
  # start_time <- 0

  start_time <- 0

  # Read data
  if(header_basic$file_spec[[1]] >= 3) {
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

  # start_time <- start_time + duration
  # start_time <- start_time + packet_header$timestamp / header_basic$time_resolution_timestamp

  start_time <- min(nsx_signal_data$timestamps)

  lapply(seq_along(idx), function(current_partition) {

    idx_range <- eval(parse(text = sprintf("c(%s)", idx[[current_partition]])))
    sel <- timestamps >= idx_range[[1]] & timestamps <= idx_range[[2]]
    time_range <- range(nsx_signal_data$timestamps[sel])

    filename <- sprintf("%s_ieeg%s%d", export_filebase, partition_prefix, current_partition)

    if( header_basic$file_spec[[1]] >= 3 ) {
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
    if(fresh_start && dir.exists(dir)) {
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


  # while( data_bytes > 0 ) {
  #   packet_header <- read_sequential(conn, packet_header_spec)
  #   packet_header <- packet_header[[1]]
  #   data_bytes <- data_bytes - header_item$.bytes
  #
  #   if( !isTRUE(packet_header$header == 1) ) {
  #     warning("NSx has invalid data header: the header must starts with 0x01. The data might be incomplete.")
  #     break
  #   }
  #
  #   n_sample_points <- packet_header$number_of_data_points
  #   if( n_sample_points <= 0 ) {
  #     warning("NSx: detected zero-length signals; skipping.")
  #     next
  #   }
  #
  #   item$n <- n_sample_points * n_channels
  #   item <- do.call(validate_spec, item)
  #
  #   if( item$.bytes > data_bytes ) {
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
  #   if(fresh_start && dir.exists(dir)) {
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

