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
  copy_images(src_dir = dirname(extr_fps), dest_dir = postdir)

  extract_ipynb(extr_fps, postdir)
}

#' @export
unzip_rmd <- function(zipfile, postdir) {
  if (!dir.exists(postdir)) stop('`', postdir, '` doesn\'t exist.')
  if (!file.exists(zipfile)) stop('`', zipfile, '` doesn\'t exist')
  postdir <- tools::file_path_as_absolute(postdir)

  temp <- tempdir()
  extr_fps <- unzip(zipfile, exdir = temp)
  copy_images(src_dir = dirname(extr_fps), dest_dir = postdir)

  # Extract `index.Rmd` to destination
  success <- extract_rmd(extr_fps, postdir)

  # Copy dependencies to destination
  if (success) {
    rmd_fps <- extr_fps[grepl('\\.[Rr]md$', extr_fps)]
    html_fps <- extr_fps[grepl('\\.html$', extr_fps)]
    rmd_dir <- dirname(rmd_fps[1])

    file.remove(c(rmd_fps, html_fps))
    file.copy(list.files(rmd_dir, full.names = TRUE), postdir,
              recursive = TRUE)
  }
}

extract_rmd <- function(extr_fps, postdir) {
  # Normal case: `index.Rmd`
  copied_fpath <- copy_file_inzip(extr_fps, file = 'index.Rmd', postdir)
  if (!is.null(copied_fpath)) return(invisible(TRUE))

  # Non-standard file name: `index.rmd`
  copied_fpath <- copy_file_inzip(extr_fps, file = 'index.rmd', postdir)
  if (!is.null(copied_fpath)) {
    file.rename(copied_fpath, paste0(dirname(copied_fpath), '/index.Rmd'))
    return(invisible(TRUE))
  }

  # `index.rmd` or `index.Rmd` doesn't exist:
     #  Use unique `*.Rmd` if exist
  copied_fpath <- copy_file_inzip(extr_fps, '.+\\.[Rr]md$', postdir, grep = TRUE)
  if (!is.null(copied_fpath)) {
    cat2('`index.Rmd` not found, searching for unique `*.Rmd` instead.\n')
    file.rename(copied_fpath, paste0(dirname(copied_fpath), '/index.Rmd'))
    return(invisible(TRUE))
  }

  warning('No file copied')
  return(FALSE)
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
  if (!is.null(copied_fpath)) return(invisible(TRUE))
  # `index.ipynb` doesn't exist:
  #  Use unique `*.ipynb` if exist
  copied_fpath <- copy_file_inzip(extr_fps, '.+\\.ipynb$', postdir, grep = TRUE)
  if (!is.null(copied_fpath)) {
    cat2('`index.ipynb` not found, searching for unique `*.ipynb` instead.\n')
    return(invisible(TRUE))
  }

  warning('No file copied')
  return(FALSE)
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
      warning('More than one `', file, '` found, use first one ',
              extr_fps[indexfile_idx][1])
      dest <- paste0(postdir, '/index.', file_ext(extr_fps[indexfile_idx][1]))
      file.copy(extr_fps[indexfile_idx][1], to = dest)  # copy text file
      file.copy(extr_fps[-indexfile_idx], to = dest)    # copy dependencies
      return(dest)
    }
    if (length(extr_fps[indexfile_idx]) == 0) {
      warning('`', file, '` not found')
    }
    return(NULL)
  }
}

file_ext <- function(...) tools::file_ext(...)
cat2 <- function(...) cat(..., sep = '')


copy_images <- function(src_dir, dest_dir) {
  tgt <- list.files(src_dir[1], pattern = '\\.(png|jpg|jpeg|svg)$', full.names = T)
  file.copy(from = tgt, to = dest_dir)
}

