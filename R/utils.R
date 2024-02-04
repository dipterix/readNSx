parse_svec <- function(text, sep = ',', connect = '-:|', sort = FALSE, unique = TRUE){
  connect <- unique(unlist(strsplit(connect, '')))
  connect[connect %in% c('|', ':', '~')] <- paste0('\\', connect[connect %in% c('|', ':', '~')])
  if('-' %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = '')

  if(length(text) != 1) {
    text <- paste(text, collapse = sep)
  }


  if(length(text) == 0 || !nzchar(trimws(text))){
    return(NULL)
  }

  if(is.numeric(text)){
    if(unique) {
      text <- unique(text)
    }
    if(sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s!='']

  s <- s[grepl(sprintf('^[0-9\\ %s]+$', connect), s)]
  # s <- s[str_detect(s, sprintf('^[0-9\\ %s]+$', connect))]

  re <- NULL
  for(ss in s){
    if(grepl(sprintf('[%s]', connect), ss)){
      ss <- unlist(strsplit(ss,  sprintf('[%s]', connect), perl = TRUE))
      # ss <- as.vector(stringr::str_split(ss, sprintf('[%s]', connect), simplify = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl('^[0-9]+$', ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if(length(ss) >= 2){
        re <- c(re, (ss[1]:ss[2]))
      }
    }else{
      re <- c(re, as.numeric(ss))
    }
  }

  if(unique){
    re <- unique(re)
  }

  if(sort){
    re <- sort(re)
  }

  return(re)
}

deparse_svec <- function(nums, connect = '-', concatenate = TRUE, collapse = ',', max_lag = 1){
  nums <- nums[is.finite(nums)]
  if(length(nums) == 0){
    return('')
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  re <- apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1,function(x){
    if(x[1] == x[2]){
      as.character(x[1])
    }else{
      paste(x, collapse = connect)
    }
  })
  if(concatenate){
    re <- paste(re, collapse = collapse)
  }
  re
}

fileexts <- function(file){
  x <- basename(file)
  sapply(strsplit(file, '\\.'), function(x){
    l <- length(x)
    ifelse(l > 1, x[[l]], '')
  })
}

backup_file <- function(path, backup_folder = dirname(path), remove = FALSE) {

  if(length(path) != 1 || is.na(path)) {
    return(invisible(FALSE))
  }
  if(!file.exists(path)){ return(invisible(FALSE)) }

  path <- normalizePath(path, mustWork = TRUE, winslash = "/")

  is_dir <- dir.exists(path)

  # find the extension
  ext <- fileexts(path)

  bname <- basename(path)

  if(ext == '') {
    bname <- gsub("[/]+$", "", bname)
  } else {
    bname <- substr(bname, start = 1L, stop = nchar(bname) - nchar(ext) - 1)
  }

  # check if bname contains timestamp
  bname <- gsub("_\\[backup_[0-9]{8}_[0-9]{6}\\]$", "", x = bname)

  bname2 <- sprintf(
    "%s_[backup_%s]%s",
    bname,
    strftime(Sys.time(), "%Y%m%d_%H%M%S"),
    ifelse(ext == "", "", sprintf(".%s", ext))
  )
  path2 <- file.path(backup_folder, bname2)

  if(!dir.exists(backup_folder)) {
    dir.create(backup_folder, showWarnings = FALSE, recursive = TRUE)
  }

  if( remove ) {
    file.rename(from = path, to = path2)
  } else {
    if(is_dir) {
      dir.create(path2, showWarnings = FALSE, recursive = TRUE)
      file.copy(
        from = list.files(
          path = path, all.files = TRUE, full.names = TRUE,
          recursive = FALSE, include.dirs = TRUE, no.. = TRUE
        ),
        to = path2, overwrite = TRUE, recursive = TRUE,
        copy.mode = TRUE, copy.date = TRUE
      )
    } else {
      file.copy(from = path, to = path2, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE, recursive = FALSE)
    }
  }

  return(invisible(path2))

}

append_table <- function(old_tbl, new_tbl, index_column = NULL, obsolete_values = NULL) {
  if(length(index_column) > 1) {
    stop("`append_table`: length of `index_column` cannot exceed 1")
  }
  if(is.data.frame(old_tbl) && nrow(old_tbl)) {
    nms <- names(new_tbl)
    col_sel <- names(old_tbl) %in% nms
    old_tbl <- as.data.frame(old_tbl)

    if(!length(index_column) || !length(obsolete_values) || !index_column %in% names(old_tbl)) {
      old_tbl <- old_tbl[, col_sel, drop = FALSE]
    } else {
      old_tbl <- old_tbl[!old_tbl[[index_column]] %in% obsolete_values, col_sel, drop = FALSE]
    }

    if(nrow(old_tbl)) {
      nms <- nms[!nms %in% names(old_tbl)]
      if(length(nms)) {
        for(nm in nms) {
          old_tbl[[nm]] <- NA
        }
      }

      new_tbl <- rbind(new_tbl, old_tbl)
    }
  }


  new_tbl

}

append_table_rds <- function(
    path, new_tbl, index_column = NULL, obsolete_values = NULL, save_tsv = TRUE) {

  if(!is.data.frame(new_tbl) || nrow(new_tbl) == 0)  { return() }

  if(file.exists(path)) {
    old_tbl <- tryCatch({
      readRDS(path)
    }, error = function(e) { NULL })
  } else {
    old_tbl <- NULL
  }
  combined_tbl <- append_table(old_tbl = old_tbl, new_tbl = new_tbl,
                               index_column = index_column, obsolete_values = obsolete_values)

  # backup files
  backup_file(path, backup_folder = file.path(dirname(path), "backups"), remove = TRUE)
  saveRDS(combined_tbl, file = path)

  if( save_tsv ) {
    tsv_path <- sprintf("%s.tsv", gsub("\\.rds$", "", x = path, ignore.case = TRUE))
    utils::write.table(
      file = tsv_path,
      x = combined_tbl,
      row.names = FALSE,
      sep = "\t",
      append = FALSE,
      col.names = TRUE
    )
  }

  invisible(combined_tbl)
}


dir_create2 <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

format_time_origin <- function( x ) {

  x <- as.list(x)
  sprintf("%04d-%02d%02dT%02d:%02d:%02d.%03d", x$Year, x$Month, x$Day, x$Hour, x$Minute, x$Second, x$Millisecond)

}

channel_filename <- function(channel_id, channel_label) {
  # no NA check
  channel_id <- as.integer(channel_id)
  sel <- !endsWith(channel_label, as.character(channel_id))
  channel_label[sel] <- sprintf("%s-%03d", channel_label[sel], channel_id[sel])
  sprintf("%s.h5", channel_label)
}

str_match <- function(x, pattern, ...) {

  idxs <- gregexpr(pattern = pattern, text = x, ...)

  unname(sapply(seq_along(x), function(ii) {
    s <- x[[ii]]
    idx <- idxs[[ii]]
    len <- attr(idx, "match.length")
    substring(s, idx, idx + len - 1L)
  }, USE.NAMES = FALSE, simplify = FALSE))

}

`%!!<-%` <- function(lhs, value) {
  env <- parent.frame()
  expr <- substitute(lhs)
  isnull <- tryCatch({
    lhs <- eval(expr, envir = env)
    is.null(lhs)
  }, error = function(e) {
    return(TRUE)
  })
  if (isnull) { return() }
  if(is.function(value)) {
    value <- value(lhs)
  }
  eval(as.call(list(quote(`<-`), expr, value)), envir = env)
}

`%?<-%` <- function (lhs, value) {
  env <- parent.frame()
  lhs <- substitute(lhs)
  isnull <- tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e) {
    return(TRUE)
  })
  if (isnull) {
    eval(as.call(list(quote(`<-`), lhs, value)), envir = env)
  }
}
