read_nev <- function( path, prefix = NULL, exclude_events = "spike", spec = NULL ) {
  # DIPSAUS DEBUG START
  # path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YEK/EMU-057_subj-YEK_task-LOCALIZER_run-01_NSP-1.nev'
  # prefix <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/bids/TestData/sub-YEK/ses-057/ieeg/sub-YEK_ses-057_task-localizer_acq-NSP1_run-01'
  # path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YDY/block058/EMU-058_subj-YDY_task-noisyAV_run-06_NSP-2.nev'
  # prefix <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/bids/TestData/sub-YDY/ses-058/ieeg/sub-YDY_ses-058_task-noisyAV_acq-NSP2_run-06'
  # ftype <- get_file_type(path = path)
  # spec <- get_specification(ftype$version, "nev")
  # exclude_events = "1-30000"

  if(missing(spec) || is.null(spec)) {
    ftype <- get_file_type(path = path)
    spec <- get_specification(ftype$version, "nev")
  } else if(!inherits(spec, "readNSx_specification_nev")) {
    stop("[readNSx::read_nev] `spec` must inherit class [readNSx_specification_nev]. If you don't know what that is, leave this argument blank or NULL")
  }

  file_name <- basename(path)
  file_size <- file.size(path)
  exclude_events <- tolower(exclude_events)

  if(!length(prefix)) {
    prefix <- normalizePath(path)
  }

  export_root <- dirname(prefix)
  export_filebase <- basename(prefix)
  dir_create2( export_root )

  conn <- file(path, "rb")
  on.exit({
    tryCatch({ close(conn) }, error = function(...){})
  }, add = TRUE, after = TRUE)


  # Basic header information
  header_basic <- structure(
    read_sequential(conn, spec$specification$section1$dictionary),
    class = c("readNSx_nev_basic_header", "readNSx_printable")
  )

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

  extra_path <- file.path(export_root, sprintf("%s_events", export_filebase))
  dir_create2(extra_path)

  for(nm in names(header_extended)) {
    tbl <- header_extended[[nm]]
    if(nm == "NEUEVLBL") {
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


  if( data_packet_bytes <= 0) {
    saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
    return( nev_data )
  }

  # for NEV, the data packets have fixed length (`data_packet_bytes`)
  # since the file is not compressed, we can estimate how many data packets
  # get estimated
  npackets <- (file_size - header_basic$bytes_in_headers) / data_packet_bytes
  if( abs(npackets - round(npackets)) > 1e-4 ) {
    warning("Number of data packets in the NEV file is not an integer. The data packets might be incomplete")
    npackets <- floor(npackets)
  } else {
    npackets <- round(npackets)
  }

  if( npackets <= 0 ) {
    saveRDS(nev_data, file = file.path(extra_path, "nev-headers.rds"))
    return( nev_data )
  }


  rules <- do.call(validate_spec, spec$specification$section3$key_rule)
  item_list <- spec$specification$section3$dictionary
  dict <- fastmap::fastmap()

  for(key in names(item_list)) {
    item <- item_list[[key]]
    if(!item$event %in% exclude_events) {
      item$name <- key
      item <- do.call(validate_spec, item)
      packet_ids <- parse_svec(key)
      lapply(packet_ids, function(id) {
        item$name <- id
        dict$set(as.character(id), item)
      })
      if(!data_packets$has( item$event )) {
        data_packets$set( item$event , fastmap::fastqueue() )
      }
    }
  }
  parse_key <- get_parse_function(rules$type)

  # Let's load all data packets at once
  buf <- readBin(conn, what = "raw", n = npackets * data_packet_bytes,
                 size = 1L, endian = "little")
  dim(buf) <- c(data_packet_bytes, npackets)

  key_idx <- rules$start_byte + seq_len(rules$.bytes)
  waveform_lut <- as.data.frame(header_extended$NEUEVWAV)
  tryCatch({
    waveform_lut <- waveform_lut[,c("electrode_id", "digitization_factor", "bytes_per_waveform")]
  }, error = function(e){})
  waveform_flag <- header_basic$additional_flags == 1
  waveform_lengths <- apply(buf, 2, function(data) {
    # DIPSAUS DEBUG START
    # data <- buf[,1]
    key <- as.character(parse_key( data[ key_idx ] ))
    if(is.na(key) || !dict$has(key)) { return(0) }
    item <- dict$get(key)

    packet <- parse_item(data, item = item)
    queue <- data_packets$get(item$event)
    value <- packet$value
    waveform_size <- 0
    tryCatch({
      if( identical(value$event, "spike") ) {

        waveform_setting <- waveform_lut[waveform_lut$electrode_id == value$packet_id, ]

        if( waveform_flag ) {
          spike_data <- parse_int16(value$waveform)
        } else {
          byte_size <- waveform_setting$bytes_per_waveform[[1]]
          if( byte_size <= 0 ) { byte_size <- 1 }
          spike_data <- switch(
            as.character(byte_size),
            "1" = {
              parse_int8(value$waveform)
            },
            "2" = {
              parse_int16(value$waveform)
            },
            "4" = {
              parse_float(value$waveform)
            },
            { stop("Cannot parse waveform byte size: ", byte_size) }
          )
        }

        value$waveform <- spike_data * waveform_setting$digitization_factor[[1]] / 1000
        waveform_size <- length(value$waveform)

      }

      if( identical(value$event, "digital_inputs") ) {
        if(length(value$packet_insertion_reason)) {
          value$packet_insertion_reason <- paste(
            value$packet_insertion_reason,
            collapse = " "
          )
        }
      }

      # remove `reserved`
      value$reserved <- NULL
      queue$add( value )
    }, error = function(e) {
      warning("Unable to add spike data, reason: ", e$message)
    })



    return(waveform_size)
  })
  waveform_maxlen <- max(waveform_lengths)

  # save data_packets
  for(event_type in data_packets$keys()) {
    queue <- data_packets$get(event_type)
    if( event_type == "spike" ) {

      nev_data$event_types <- c(nev_data$event_types, "spike")

      # needs to remove this type: data is too big
      data_packets$remove( event_type )

      waveform_data <- sapply(seq_len(queue$size()), function(ii) {
        spike <- queue$remove()
        waveform <- spike$waveform
        if(length(waveform) < waveform_maxlen) {
          waveform <- c(waveform, rep(0, waveform_maxlen - length(waveform)))
        }
        c(
          spike$packet_id,
          spike$timestamp / header_basic$time_resolution_timestamp,
          spike$unit_classification_number,
          waveform
        )
      })

      waveform_path <- file.path(extra_path, "waveforms.h5")

      save_h5(x = format_time_origin( header_basic$time_origin ), file = waveform_path, name = "timeOrigin", replace = TRUE, ctype = "character", quiet = TRUE, chunk = 1L)

      save_h5(x = as.integer(waveform_data[1, ]), file = waveform_path, name = "channelID", replace = TRUE, level = 9, ctype = "integer", quiet = TRUE, chunk = 16384L)

      save_h5(x = as.double(waveform_data[2, ]), file = waveform_path, name = "timeInSeconds", replace = TRUE, level = 0, ctype = "numeric", quiet = TRUE, chunk = 16384L)

      save_h5(x = as.integer(waveform_data[3, ]), file = waveform_path, name = "classification", replace = TRUE, level = 9, ctype = "integer", quiet = TRUE, chunk = 16384L)

      if( waveform_maxlen > 0 ) {
        save_h5(x = as.double(waveform_data[-(1:3), ]), file = waveform_path, name = "waveform", replace = TRUE, level = 0, ctype = "numeric", quiet = TRUE)
      }

    } else if( queue$size() > 0 ) {

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

