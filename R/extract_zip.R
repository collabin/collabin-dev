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

  # Remove possibly complicating hidden files
  hidden_f_idx <- grepl('\\.ipynb_checkpoints', extr_fps)
  if (sum(hidden_f_idx) >= 1) {
    extr_fps <- extr_fps[!hidden_f_idx]
  }

  # Check if `index.ipynb` exist
  indexfile_idx <- basename(extr_fps) == 'index.ipynb'
  if (length(extr_fps[indexfile_idx]) == 1) {
    file.copy(extr_fps[indexfile_idx], to = postdir)
    cat('Found `index.ipynb`\nCopied to', postdir, '\n')
    invisible()
  }

  # `index.ipynb` doesn't exist:
  #  Use unique `*.ipynb` if exist
  ipynb_idx <- grepl('^[a-zA-Z0-9_-]+\\.ipynb$', basename(extr_fps))
  if (length(extr_fps[ipynb_idx]) == 0) stop('No `.ipynb` file in zip file')
  if (length(extr_fps[ipynb_idx]) > 1) stop('More than 1 `.ipynb` exist')
  cat('Found `', basename(extr_fps[ipynb_idx]), '`\n',
      'Copied to `', postdir, '/index.ipynb`\n', sep = '')
  file.copy(extr_fps[ipynb_idx], to = paste0(postdir, '/index.ipynb'))
  invisible()
}
