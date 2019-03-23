recode_as_utf8 <- function(file, outfile = file) {
  file_encoding <- readr::guess_encoding(file)$encoding[1]
  if (file_encoding == 'UTF-8') return(invisible(TRUE))

  x <- readLines(file, encoding = file_encoding)
  x <- iconv(x, from = file_encoding, to = 'UTF-8')
  writeLines(x, outfile)
}

