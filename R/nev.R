# Reads the basic and extended headers; `conn` must be at the beginning of the
# file and is left at the first data packet
read_nev_headers <- function(conn, spec) {

  # Basic header information
  header_basic <- structure(
    read_sequential(conn, spec$specification$section1$dictionary),
    class = c("readNSx_nev_basic_header", "readNSx_printable")
  )

  # Fixing NEV 3.0 file bug
  if (identical(paste(header_basic$file_spec, collapse = "."), "3.0")) {
    if ( header_basic$time_resolution_timestamp < 30000 ) {
      header_basic$time_resolution_timestamp <- 30000
    }
  }

  # Get extended header information
  header_extended <- structure(
    read_keyvalue_pairs(
      conn = conn,
      rules = spec$specification$section2$key_rule,
      item_list = spec$specification$section2$dictionary,
      expected_items = header_basic$number_of_extended_headers
    ),
    class = c("readNSx_nev_extended_header", "readNSx_printable")
  )

  list(
    header_basic = header_basic,
    header_extended = header_extended
  )
}

#' @title Read 'NEV' headers
#' @description Reads the basic and extended headers of a 'NEV' file, such as
#' the channel labels and waveform settings, without reading the data packets
#' and without writing any file. Unlike \code{\link{import_nsp}}, the time
#' needed does not grow with the length of the recording.
#' @param path path to the 'NEV' file
#' @returns A list of \code{header_basic} (file settings such as the time
#' resolution and the data packet size) and \code{header_extended} (one data
#' frame per extended header type, for example \code{NEUEVLBL} for channel
#' labels and \code{NEUEVWAV} for waveform settings). See
#' \code{\link{get_specification}} for the fields.
#' @examples
#'
#' # Please get your own sample data first. This package does not
#' # provide sample data for privacy and license concerns :)
#'
#' if(interactive() && file.exists("sampledata.nev")) {
#'
#' header <- read_nev_header("sampledata.nev")
#'
#' # channel labels
#' header$header_extended$NEUEVLBL
#'
#' }
#'
#' @export
read_nev_header <- function(path) {
  ftype <- get_file_type(path = path)
  if (!identical(ftype$type, "nev")) {
    stop("[readNSx::read_nev_header] `path` is not a NEV file: ", path)
  }
  spec <- get_specification(ftype$version, "nev")

  conn <- file(path, "rb")
  on.exit({
    tryCatch({ close(conn) }, error = function(...) {})
  }, add = TRUE, after = TRUE)

  read_nev_headers(conn, spec)
}

# Bytes per waveform sample and digitization factor of every electrode,
# indexed by `electrode_id + 1`; `NA` when spikes of the electrode cannot be
# parsed (no `NEUEVWAV` header or an unknown sample size)
nev_waveform_settings <- function(header_basic, header_extended) {
  sample_bytes <- rep(NA_integer_, 65536L)
  digitization <- rep(NA_integer_, 65536L)
  settings <- header_extended$NEUEVWAV
  columns <- c("electrode_id", "digitization_factor", "bytes_per_waveform")
  if (is.data.frame(settings) && all(columns %in% names(settings))) {
    settings <- settings[!duplicated(settings$electrode_id), ]
    bytes <- as.integer(settings$bytes_per_waveform)
    bytes[bytes <= 0] <- 1L
    if ( isTRUE(header_basic$additional_flags == 1) ) {
      # all spike waveforms are 16-bit
      bytes[] <- 2L
    }
    bytes[!bytes %in% c(1L, 2L, 4L)] <- NA_integer_
    sample_bytes[settings$electrode_id + 1L] <- bytes
    digitization[settings$electrode_id + 1L] <- settings$digitization_factor
  }
  list(sample_bytes = sample_bytes, digitization = digitization)
}

# Byte layout of spike packets: timestamp, packet ID (electrode), unit
# classification, one reserved byte, then the waveform up to the end of the
# packet (the specification lists only the first bytes of the waveform)
nev_spike_layout <- function(spike_item, data_packet_bytes) {
  field_bytes <- vapply(names(spike_item$specs), function(name) {
    do.call(validate_spec, c(list(name = name), spike_item$specs[[name]]))$.bytes
  }, FUN.VALUE = 0)
  field_offset <- cumsum(c(0, field_bytes))[seq_along(field_bytes)]
  names(field_offset) <- names(field_bytes)
  list(
    timestamp_rows = field_offset[["timestamp"]] +
      seq_len(field_bytes[["timestamp"]]),
    unit_row = field_offset[["unit_classification_number"]] + 1L,
    waveform_offset = field_offset[["waveform"]],
    waveform_bytes = data_packet_bytes - field_offset[["waveform"]]
  )
}

read_nev <- function( path, prefix = NULL, exclude_events = "spike", spec = NULL,
                      chunk_bytes = 16 * 1024^2 ) {
  # DIPSAUS DEBUG START
  # path = "/Users/dipterix/rave_data/raw_dir/PAV023/BLOCK024_mTurkWords_run1/PAV023_Datafile_024.nev"
  # prefix = "/Users/dipterix/rave_data/raw_dir/PAV023/BLOCK024_mTurkWords_run1/extraction/data"
  # ftype <- get_file_type(path = path)
  # spec <- get_specification(ftype$version, "nev")
  # exclude_events = "spike"

  if (missing(spec) || is.null(spec)) {
    ftype <- get_file_type(path = path)
    spec <- get_specification(ftype$version, "nev")
  } else if (!inherits(spec, "readNSx_specification_nev")) {
    stop("[readNSx::read_nev] `spec` must inherit class [readNSx_specification_nev]. If you don't know what that is, leave this argument blank or NULL")
  }

  file_name <- basename(path)
  file_size <- file.size(path)
  exclude_events <- tolower(exclude_events)

  if (!length(prefix)) {
    prefix <- normalizePath(path)
  }

  export_root <- dirname(prefix)
  export_filebase <- basename(prefix)
  dir_create2( export_root )

  conn <- file(path, "rb")
  on.exit({
    tryCatch({ close(conn) }, error = function(...) {})
  }, add = TRUE, after = TRUE)

  headers <- read_nev_headers(conn, spec)
  header_basic <- headers$header_basic
  header_extended <- headers$header_extended

  # Export header_basic as _scans
  tbl <- data.frame(
    filename = sprintf("%s_ieeg", export_filebase),
    acq_time = format_time_origin( header_basic$time_origin ),
    original_filename = file_name,
    file_type = header_basic$file_type,
    file_spec = paste(header_basic$file_spec, collapse = "."),
    application = header_basic$application_to_create_file,

    # global sample rate (max possible)
    # for NSx, time_resolution_timestamp / period is the signal sample rate
    time_resolution_timestamp = header_basic$time_resolution_timestamp,

    # waveform sample rate
    time_resolution_waveforms = header_basic$time_resolution_samples,
    bytes_in_data_packet = header_basic$bytes_in_data_packet,
    comment = paste(header_basic$comment, collapse = "")
  )
  append_table_rds(
    path = file.path(export_root, sprintf("%s_scans.rds", export_filebase)),
    new_tbl = tbl, index_column = "original_filename",
    obsolete_values = file_name)

  extra_path <- file.path(export_root, sprintf("%s_events", export_filebase))
  dir_create2(extra_path)

  for (nm in names(header_extended)) {
    tbl <- header_extended[[nm]]
    if (nm == "NEUEVLBL") {
      tbl$filename <- channel_filename(
        channel_id = tbl$electrode_id,
        channel_label = tbl$label)
    }
    tbl$original_filename <- file_name
    append_table_rds(
      path = file.path(extra_path, sprintf("%s.rds", nm)),
      new_tbl = tbl, index_column = "original_filename",
      obsolete_values = file_name)
  }

  # read data packets
  data_packet_bytes <- header_basic$bytes_in_data_packet
  data_packets <- fastmap::fastmap()

  nev_data <- structure(
    list(
      specification = spec,
      header_basic = header_basic,
      header_extended = header_extended,
      # data_packets = structure(
      #   data_packets,
      #   class = c("readNSx_nev_data_packets", "readNSx_printable", "list")
      # ),
      event_types = NULL,
      prefix = prefix
    ),
    exclude_events = exclude_events,
    class = c(
      "readNSx_nev",
      "readNSx_printable"
    )
  )


  if ( data_packet_bytes <= 0) {
    saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
    return( nev_data )
  }

  # for NEV, the data packets have fixed length (`data_packet_bytes`)
  # since the file is not compressed, we can estimate how many data packets
  # get estimated
  npackets <- (file_size - header_basic$bytes_in_headers) / data_packet_bytes
  if ( abs(npackets - round(npackets)) > 1e-4 ) {
    warning("Number of data packets in the NEV file is not an integer. The data packets might be incomplete")
    npackets <- floor(npackets)
  } else {
    npackets <- round(npackets)
  }

  if ( npackets <= 0 ) {
    saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
    return( nev_data )
  }


  rules <- do.call(validate_spec, spec$specification$section3$key_rule)
  item_list <- spec$specification$section3$dictionary
  dict <- fastmap::fastmap()

  # event of every packet ID that is not excluded; packet IDs are `uint16`
  event_lut <- character(65536L)
  spike_item <- NULL

  for (key in names(item_list)) {
    item <- item_list[[key]]
    if (!item$event %in% exclude_events) {
      item$name <- key
      item <- do.call(validate_spec, item)
      packet_ids <- parse_svec(key)
      event_lut[packet_ids + 1L] <- item$event
      if (item$event == "spike") {
        # spikes are parsed for a whole chunk at once (see below)
        spike_item <- item
      } else {
        lapply(packet_ids, function(id) {
          item$name <- id
          dict$set(as.character(id), item)
        })
      }
      if (!data_packets$has( item$event )) {
        data_packets$set( item$event, fastmap::fastqueue() )
      }
    }
  }

  if ( !data_packets$size() ) {
    # every event is excluded: no need to read the data packets
    saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
    return( nev_data )
  }

  if ( !identical(rules$type, "uint16") ) {
    stop("[readNSx::read_nev] Unsupported NEV data packet ID type: ", rules$type)
  }
  # rows of the two (little-endian) packet ID bytes in a column of packets
  id_rows <- rules$start_byte + c(1L, 2L)
  chunk_packets <- max(floor(chunk_bytes / data_packet_bytes), 1)

  # Reads the packets in chunks (one packet per column) and calls
  # `FUN(buf, ids)`, where `ids` are the packet IDs of the chunk; `conn` must
  # be at the first data packet
  for_each_chunk <- function(conn, FUN) {
    remaining <- npackets
    while (remaining > 0) {
      n <- min(chunk_packets, remaining)
      buf <- readBin(conn, what = "raw", n = n * data_packet_bytes,
                     size = 1L, endian = "little")
      dim(buf) <- c(data_packet_bytes, n)
      ids <- as.integer(buf[id_rows[[1]], ]) +
        256L * as.integer(buf[id_rows[[2]], ])
      FUN(buf, ids)
      remaining <- remaining - n
    }
  }

  # Non-spike events are rare: parse them packet by packet
  add_event_packet <- function(data, id) {
    item <- dict$get(as.character(id))
    value <- parse_item(data, item = item)$value
    tryCatch({
      if ( identical(value$event, "digital_inputs") ) {
        if (length(value$packet_insertion_reason)) {
          value$packet_insertion_reason <- paste(
            value$packet_insertion_reason,
            collapse = " "
          )
        }
      }

      # remove `reserved`
      value$reserved <- NULL
      data_packets$get(item$event)$add( value )
    }, error = function(e) {
      warning("Unable to add NEV event data, reason: ", e$message)
    })
    invisible()
  }

  # Spikes are counted here and written to disk in a second pass
  if (!is.null(spike_item)) {
    waveform_settings <- nev_waveform_settings(header_basic, header_extended)
    spike_layout <- nev_spike_layout(spike_item, data_packet_bytes)
  }
  n_spikes <- 0
  n_spikes_skipped <- 0
  spike_electrodes <- integer(0L)

  for_each_chunk(conn, function(buf, ids) {
    events <- event_lut[ids + 1L]
    is_spike <- events == "spike"
    if (any(is_spike)) {
      usable <- is_spike &
        !is.na(waveform_settings$sample_bytes[ids + 1L])
      n_spikes <<- n_spikes + sum(usable)
      n_spikes_skipped <<- n_spikes_skipped + sum(is_spike & !usable)
      spike_electrodes <<- unique(c(spike_electrodes, ids[usable]))
    }
    for (ii in which(nzchar(events) & !is_spike)) {
      add_event_packet(buf[, ii], ids[[ii]])
    }
  })

  if ( n_spikes_skipped > 0 ) {
    warning(sprintf(
      "Unable to add %.0f spike packet(s): their electrodes have no valid waveform settings (NEUEVWAV) in the NEV header",
      n_spikes_skipped))
  }

  # save data_packets
  for (event_type in data_packets$keys()) {
    queue <- data_packets$get(event_type)
    if ( event_type == "spike" ) {

      # needs to remove this type: data is too big
      data_packets$remove( event_type )

      waveform_path <- file.path(extra_path, "waveforms.h5")
      if ( file.exists(waveform_path) ) {
        unlink(waveform_path)
      }
      if ( n_spikes == 0 ) { next }

      nev_data$event_types <- c(nev_data$event_types, "spike")

      # longest waveform: electrodes with the smallest samples
      waveform_maxlen <- max(
        spike_layout$waveform_bytes %/%
          waveform_settings$sample_bytes[spike_electrodes + 1L])

      save_nev_spikes(
        path = path, spec = spec, waveform_path = waveform_path,
        header_basic = header_basic, n_spikes = n_spikes,
        waveform_maxlen = waveform_maxlen, layout = spike_layout,
        settings = waveform_settings, event_lut = event_lut,
        for_each_chunk = for_each_chunk)

    } else if ( queue$size() > 0 ) {

      nev_data$event_types <- c(nev_data$event_types, event_type)

      tbl <- data.table::rbindlist(queue$as_list(), fill = TRUE)
      tbl$time_in_seconds <- tbl$timestamp / header_basic$time_resolution_timestamp
      tbl$original_filename <- file_name

      append_table_rds(
        path = file.path(extra_path, sprintf("event-%s.rds", event_type)),
        new_tbl = tbl, index_column = "original_filename",
        obsolete_values = file_name)
    }
  }

  saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
  return( nev_data )
}

# Writes the spikes to `waveform_path`: channel ID, time in seconds, unit
# classification, and the waveform in micro-volts (one column per spike,
# zero-padded to `waveform_maxlen` samples), all in file order. The spikes of
# each chunk are parsed at once and written straight into pre-allocated
# datasets, so memory use does not grow with the number of spikes.
save_nev_spikes <- function(path, spec, waveform_path, header_basic, n_spikes,
                            waveform_maxlen, layout, settings, event_lut,
                            for_each_chunk) {

  save_h5(x = format_time_origin( header_basic$time_origin ), file = waveform_path, name = "timeOrigin", replace = TRUE, ctype = "character", quiet = TRUE, chunk = 1L)

  allocate_h5(file = waveform_path, name = "channelID", dims = n_spikes, chunk = 16384L, level = 9, ctype = "integer", quiet = TRUE)

  allocate_h5(file = waveform_path, name = "timeInSeconds", dims = n_spikes, chunk = 16384L, level = 0, ctype = "numeric", quiet = TRUE)

  allocate_h5(file = waveform_path, name = "classification", dims = n_spikes, chunk = 16384L, level = 9, ctype = "integer", quiet = TRUE)

  if ( waveform_maxlen > 0 ) {
    allocate_h5(file = waveform_path, name = "waveform", dims = c(waveform_maxlen, n_spikes), level = 0, ctype = "numeric", quiet = TRUE)
  }

  # little-endian unsigned timestamps (32 or 64 bits)
  timestamp_weights <- 256^(seq_along(layout$timestamp_rows) - 1)

  conn <- file(path, "rb")
  on.exit({
    tryCatch({ close(conn) }, error = function(...) {})
  }, add = TRUE, after = TRUE)
  # move to the first data packet
  read_nev_headers(conn, spec)

  written <- 0
  for_each_chunk(conn, function(buf, ids) {
    spikes <- which(
      event_lut[ids + 1L] == "spike" &
        !is.na(settings$sample_bytes[ids + 1L])
    )
    if (!length(spikes)) { return() }
    channel <- ids[spikes]

    timestamp <- buf[layout$timestamp_rows, spikes, drop = FALSE]
    timestamp <- colSums(
      array(as.numeric(timestamp), dim = dim(timestamp)) * timestamp_weights)

    if ( waveform_maxlen > 0 ) {
      waveform <- matrix(0, nrow = waveform_maxlen, ncol = length(spikes))
      sample_bytes <- settings$sample_bytes[channel + 1L]
      for (size in unique(sample_bytes)) {
        columns <- which(sample_bytes == size)
        n_samples <- layout$waveform_bytes %/% size
        if (n_samples <= 0) { next }
        bytes <- as.vector(buf[layout$waveform_offset + seq_len(n_samples * size),
                               spikes[columns], drop = FALSE])
        samples <- switch(
          as.character(size),
          "1" = readBin(bytes, what = "integer", n = length(bytes), size = 1L,
                        signed = TRUE),
          "2" = readBin(bytes, what = "integer", n = length(bytes) / 2, size = 2L,
                        signed = TRUE, endian = "little"),
          "4" = readBin(bytes, what = "numeric", n = length(bytes) / 4, size = 4L,
                        endian = "little")
        )
        digitization <- rep(settings$digitization[channel[columns] + 1L],
                            each = n_samples)
        waveform[seq_len(n_samples), columns] <- samples * digitization / 1000
      }
      write_h5_slice(x = waveform, file = waveform_path, name = "waveform", start = c(1, written + 1), quiet = TRUE)
    }

    write_h5_slice(x = as.integer(channel), file = waveform_path, name = "channelID", start = written + 1, quiet = TRUE)
    write_h5_slice(x = timestamp / header_basic$time_resolution_timestamp, file = waveform_path, name = "timeInSeconds", start = written + 1, quiet = TRUE)
    write_h5_slice(x = as.integer(buf[layout$unit_row, spikes]), file = waveform_path, name = "classification", start = written + 1, quiet = TRUE)

    written <<- written + length(spikes)
  })

  if ( written != n_spikes ) {
    stop("[readNSx::read_nev] Expected ", n_spikes, " spikes, but wrote ",
         written, ". The NEV file might have changed while being read.")
  }
  invisible(waveform_path)
}
