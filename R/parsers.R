get_parse_function <- function(type) {
  get(sprintf("parse_%s", type), mode = "function", envir = asNamespace("readNSx"))
}

parse_uint8 <- function(x, ...) {
  rawToUInt8(x)
}
parse_int8 <- function(x, ...) {
  rawToInt8(x)
}
parse_uint16 <- function(x, ...) {
  rawToUInt16(x)
}
parse_int16 <- function(x, ...) {
  rawToInt16(x)
}
parse_uint32 <- function(x, ...) {
  rawToUInt32(x)
}
parse_int32 <- function(x, ...) {
  rawToInt32(x)
}
parse_uint64 <- function(x, ...) {
  # There is no R data type that can hold uint64
  # luckily, blackrock uses uint64 to store timestamp, which
  # should not exceed the half limit, and int64 should suffice
  if(!requireNamespace("bit64")) {
    warning("The data contains integer64 type, which is not supported by native R. Please install `bit64` package via\n  install.packages('bit64')")
  }
  rawToInt64(x)
}
parse_int64 <- function(x, ...) {
  if(!requireNamespace("bit64")) {
    warning("The data contains integer64 type, which is not supported by native R. Please install `bit64` package via\n  install.packages('bit64')")
  }
  rawToInt64(x)
}
parse_float <- function(x, ...) {
  rawToFloat(x)
}

parse_string <- function(x, ...) {
  rawToString(x)
}
parse_bit <- function(x, ...) {
  rawToBits(x)
}
parse_raw <- function(x, ...) {
  x
}
parse_reserved <- function(x, ...) {
  return()
}
parse_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  names(packet) <- names
  if(length(item$event)) {
    packet$event <- item$event
  }
  packet
}
parse_comment_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  if(is.character(packet[[length(packet)]]) && length(x) > length(idx)) {
    s <- parse_string(x[-seq_len(idx)])
    packet[[length(packet)]] <- paste0(packet[[length(packet)]], s)
  }
  names(packet) <- names
  packet$event <- item$event
  packet
}


parse_item <- function(slice_data, item) {
  # item <- section_specs[[ii]]
  # slice_idx <- section_slice_idx[ii, ]
  # slice_data <- section_data[seq(slice_idx[[1]], slice_idx[[2]])]
  parser <- get_parse_function(item$type)
  if(!is.function(parser)) {
    stop("Cannot obtain parser function for type: ", item$type)
  }
  if(item$n > 1) {
    re <- matrix(slice_data, ncol = item$n, byrow = FALSE)
    re <- apply(re, 2, parser, item = item)
  } else {
    re <- parser(slice_data, item = item)
  }

  if(length(item$names)) {
    names(re) <- item$names
  }
  structure(
    list(
      name = item$name,
      raw = slice_data,
      value = re
    ),
    class = c(
      sprintf("nev-nsx-%s", item$name),
      "nev-nsx-entry"
    )
  )
}

read_sequential <- function(conn, item_list) {
  keys <- names(item_list)
  re <- fastmap::fastmap()
  lapply(keys, function(name) {
    item <- item_list[[name]]
    item$name <- name
    item <- do.call(validate_spec, item)

    data <- readBin(conn, what = "raw", n = item$size * item$n,
                    size = 1L, endian = "little")

    parsed <- parse_item(slice_data = data, item = item)
    re$set(name, parsed$value)
    return()
  })
  re$as_list()
}

read_keyvalue_pairs <- function(conn, rules, item_list, expected_items, as_data_frame = TRUE) {

  # DIPSAUS DEBUG START
  # try({ close(conn) }, silent = TRUE)
  # path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YEK/EMU-057_task-LOCALIZER_run-01/EMU-057_subj-YEK_task-LOCALIZER_run-01_NSP-1.nev'
  # conn <- file(path, "rb")
  # ftype <- get_file_type(path = path)
  # spec <- get_specification(ftype$version, "nev")
  # rules <- spec$specification$section2$key_rule
  # item_list <- spec$specification$section2$dictionary
  # header_basic <- read_sequential(conn, spec$specification$section1$dictionary)
  # rules <- do.call(validate_spec, rules)
  # parse_key <- get(sprintf("parse_%s", rules$type), mode = "function")
  # expected_items <- header_basic$number_of_extended_headers
  # ii <- 1
  rules <- do.call(validate_spec, rules)
  parse_key <- get_parse_function(rules$type)

  dict <- fastmap::fastmap()
  re <- fastmap::fastmap()

  # minimum bytes needed to get key
  initial_bytes <- rules$start_byte + rules$.bytes
  lapply(seq_len(expected_items), function(ii) {

    # read `initial_bytes` to get key
    buf <- readBin(conn, what = "raw", n = initial_bytes, size = 1L, endian = "little")

    key <- parse_key(buf[ rules$start_byte + seq_len(rules$.bytes) ])

    item <- dict$get(key, missing = local({
      item <- item_list[[key]]
      if(is.null(item)) {
        stop("Cannot find specification for keyword: [", key, "]")
      }
      item$name <- key
      item <- do.call(validate_spec, item)
      dict$set(key, item)
      item
    }))

    rest_length <- item$.bytes - initial_bytes
    if( rest_length < 0 ) {
      stop("Wrong specification: data packet size is not enough to aquire packet key/ID. To obtain the key, it requires [", initial_bytes, "] bytes, but the packet size is: [", item$.bytes, "]")
    }

    buf <- c(buf, readBin(conn, what = "raw", n = rest_length, size = 1L, endian = "little"))

    packet <- parse_item(slice_data = buf, item = item)

    if(!re$has(key)) {
      queue <- fastmap::fastqueue()
      re$set(key = key, value = queue)
    } else {
      queue <- re$get(key)
    }
    queue$add(packet$value)
    return()
  })

  # turn results into data.frame
  if( as_data_frame ) {
    keys <- re$keys()
    re <- structure(
      lapply(keys, function(key) {
        queue <- re$get(key)
        data.table::rbindlist(queue$as_list(), use.names = TRUE, fill = TRUE)
      }),
      names = keys
    )
  }

  return( re )

}

