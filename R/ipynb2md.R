#' Convert Jupyter Notebook to Markdown
#'
#' Expected to work in dir where the source \code{.ipynb}
#'   is available.
#' @export
ipynb2md <- function(ipynb = 'index.ipynb',
                     docker = 'jgoldfar/pandoc-nbconvert-docker') {
  stopifnot(file.exists(ipynb))

  # Temporary chage working dir to where `.ipynb` is
  ori_dir <- getwd()
  setwd(dirname(ipynb))
  ipynb <- basename(ipynb)
  on.exit(expr = {setwd(ori_dir)}, add = TRUE)

  # Call docker to use nbconvert
  mount <- '`pwd`:/source'
  system2('docker',
          args = c('run', '--rm', '-v', mount, '--user', '$(id -u):$(id -g)', docker, 'nbconvert', ipynb, '--to', 'markdown'))
}

# docker run --rm -v `pwd`:/source --user $(id -u):$(id -g) liao961120/pandoc-nbconvert nbconvert article.ipynb --to markdown
