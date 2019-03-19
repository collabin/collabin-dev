#' Unzip, find, and copy .ipynb to destination folder
#'
#' @param zipfile String. Path to zip archive.
#' @param postdir String. Path to target directory.
#'
#' @export
unzip_ipynb <- function(zipfile, postdir) {
  if (!dir.exists(postdir)) stop('`', postdir, '` doesn\'t exist.')
  if (!file.exists(zipfile)) stop('`', zipfile, '` doesn\'t exist')
  postdir <- tools::file_path_as_absolute(postdir)

  temp <- tempdir()
  extr_fps <- unzip(zipfile, exdir = temp)

  extract_ipynb(extr_fps, postdir)
}

#' @export
unzip_rmd <- function(zipfile, postdir) {
  if (!dir.exists(postdir)) stop('`', postdir, '` doesn\'t exist.')
  if (!file.exists(zipfile)) stop('`', zipfile, '` doesn\'t exist')
  postdir <- tools::file_path_as_absolute(postdir)

  temp <- tempdir()
  extr_fps <- unzip(zipfile, exdir = temp)

  extract_rmd(extr_fps, postdir)
}

extract_rmd <- function(extr_fps, postdir) {
  # Normal case: `index.Rmd`
  copied_fpath <- copy_file_inzip(extr_fps, file = 'index.Rmd', postdir)
  if (!is.null(copied_fpath)) return(invisible())

  # Non-standard file name: `index.rmd`
  copied_fpath <- copy_file_inzip(extr_fps, file = 'index.rmd', postdir)
  if (!is.null(copied_fpath)) {
    file.rename(copied_fpath, paste0(dirname(copied_fpath), '/index.Rmd'))
    return(invisible())
  }

  # `index.rmd` or `index.Rmd` doesn't exist:
     #  Use unique `*.Rmd` if exist
  copied_fpath <- copy_file_inzip(extr_fps, '^[a-zA-Z0-9_-]+\\.[Rr]md$', postdir, grep = TRUE)
  if (!is.null(copied_fpath)) {
    cat2('`index.Rmd` not found, searching for unique `*.Rmd` instead.\n')
    file.rename(copied_fpath, paste0(dirname(copied_fpath), '/index.Rmd'))
    return(invisible())
  }
  warning('No file copied')
  return(copied_fpath)
}

extract_ipynb <- function(extr_fps, postdir) {
  # Remove possibly complicating hidden files
  hidden_f_idx <- grepl('\\.ipynb_checkpoints', extr_fps)
  if (sum(hidden_f_idx) >= 1) {
    extr_fps <- extr_fps[!hidden_f_idx]
  }

  # Check if `index.ipynb` exist,
  # if it does, copy it to `postdir` & terminate function
  copied_fpath <- copy_file_inzip(extr_fps, file = 'index.ipynb', postdir)
  if (!is.null(copied_fpath)) return(invisible())
  # `index.ipynb` doesn't exist:
  #  Use unique `*.ipynb` if exist
  copied_fpath <- copy_file_inzip(extr_fps, '^[a-zA-Z0-9_-]+\\.ipynb$', postdir, grep = TRUE)
  if (!is.null(copied_fpath)) {
    cat2('`index.ipynb` not found, searching for unique `*.ipynb` instead.\n')
    return(invisible())
  }

  warning('No file copied')
  return(copied_fpath)
}


copy_file_inzip <- function(extr_fps, file = 'index.Rmd', postdir, grep = FALSE) {
  indexfile_idx <- grepl(file, basename(extr_fps), fixed = !grep)

  if (length(extr_fps[indexfile_idx]) == 1) {
    dest <- paste0(postdir, '/index.', file_ext(extr_fps[indexfile_idx]))

    file.copy(extr_fps[indexfile_idx], to = dest)
    cat2('Found `', extr_fps[indexfile_idx], '`\nCopied to `', dest, '` \n')
    return(dest)
  } else {
    if (length(extr_fps[indexfile_idx]) > 1) {
      warning('More than one `', file, '` found')
    }
    if (length(extr_fps[indexfile_idx]) == 0) {
      warning('`', file, '` not found')
    }
    return(NULL)
  }
}

file_ext <- function(...) tools::file_ext(...)
cat2 <- function(...) cat(..., sep = '')
