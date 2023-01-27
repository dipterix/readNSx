
validate_spec <- function(name, type, size, n = 1, names = NULL, ...) {
  # check type first
  size_ <- byte_size_lut[[type]]
  if(!missing(size) && length(size) == 1) {
    if(!is.null(size_) && !size %in% size_) {
      stop("Cannot parse name [", name, "]: the type [", type, "] cannot have ", size, " bytes.")
    }
    size_ <- size
  } else if (!length(size_)) {
    stop("Cannot parse name [", name, "]: unknown size for data type: ", type)
  } else {
    size_ <- size_[[1]]
  }
  if(!length(n)) {
    n <- 1L
  } else {
    n <- as.integer(n)
    if (length(n) > 1 || any(is.na(n))) {
      stop("Element length `n` must be an integer")
    }
  }

  re <- list(...)
  re$name <- name
  re$type <- type
  re$size <- size_
  re$n <- n
  re$names <- names
  re$.bytes <- n * size_
  return(re)
}
