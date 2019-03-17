#' Auto attach yaml header to md based on 1 input row of df
#'
#' @param df A data frame with one row.
#' @param author_yml_dir String. Path where yaml of author information
#'   are stored.
#' @export
gsheet2post <- function(df,  post_dir_name = NULL,
                        author_yml_dir = 'content/author_info/') {
  stopifnot(nrow(df) == 1)

  vars <- c('date', 'slack', 'zip_path', 'format',
            'shorturl', 'title', 'subtitle', 'tags')
  gs_yaml <- gs2yaml(df, vars)

  author_id <- lookup_id(gs_yaml$slack)

  stopifnot(file.exists(paste0(author_yml_dir, author_id, '.yml')))

  # Construct post yaml header
  post_yaml <- yaml4post(author_id,
                         gs_yaml[c('title','subtitle','tags','date')],
                         author_yml_dir)

  # Move working dir to prepare for modifying md
  ori_dir <- getwd()
  if (is.null(post_dir_name) && is.na(gs_yaml$shorturl)) {
    stop('No post dir provided')
  }
  post_dir_name <- ifelse(is.null(post_dir_name),
                     gs_yaml$shorturl,
                     post_dir_name)
  post_dir <- paste('content', author_id, post_dir_name, sep = '/')
  #setwd(post_dir)
  #on.exit({setwd(ori_dir)}, add = TRUE)

  # Attach constructed yaml header to md
  post_file <- ifelse(gs_yaml$format == '.Rmd',
                      '/index.Rmd', '/index.md')
  attach_yaml2md(post_yaml, md = paste0(post_dir, post_file))
}



lookup_id <- function(slack, path = 'content/author_info/id.yml') {
  slack <- normalize(slack)
  lookup_lst <- read_yaml(path)
  author_info <- lookup_lst[slack][[1]]
  return(author_info$id)
}
