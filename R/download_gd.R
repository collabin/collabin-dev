#' Download Google Drive file with URL
#'
#' The file is saved as a temporary file on the system.
#'
#' @param url String. URL to where the file is save.
#'   The URL may be in either
#'   \code{https://drive.google.com/open?id=<file-id>} or
#'   \code{https://drive.google.com/uc?export=download&id=<file-id>}
#'   format.
#'
#' @return A list with \code{fpath}, the path to the download file
#'   and \code{isZip}, whether the downloaded file is zip file.
#' @export
download_gd <- function(url) {
  temp <- tempfile()
  url <- gsub('https://drive.google.com/open?id=',
              'https://drive.google.com/uc?export=download&id=',
              zip_url, fixed = TRUE)
  url <- paste0('"', url, '"')

  system2('curl', args = c('-L', '-o', temp, url))
  file_type <- system2('file', temp, stdout = T)

  cat2('File downloaded to `', temp, '`')
  return(list(fpath = temp, isZip = grepl('Zip', file_type)))
}

# Alternative solution for downloading big files from GD
# https://medium.com/@afun/downloading-big-file-form-google-drive-with-curl-7918bc3b2605


