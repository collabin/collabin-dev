#' Auto attach yaml header to md based on 1 input row of df
#'
#' @param df A data frame with one row.
#' @param post_dir_name String. The name of the directory of the
#'   target post. Defaults to \code{NULL}. In this case, the value
#'   of \code{post_dir_name} will be automatically extracted from
#'   the column \code{shorturl} of the first argument \code{df}.
#' @param lookup_id A function. Defaults to
#'   \code{collabin:::lookup_lope_id}. This function will be used
#'   to map the variable \code{id} in \code{df} to the name of the
#'   directory of the author. The file
#'   \code{content/author_info/id.yml} defines this mapping
#'   relationship.
#'   The toppest level of the entries in \code{id.yml}
#'   corresponds to the \code{id} column in the passed-in \code{df}.
#'   The entries \code{id} under the toppest level entries are the
#'   mapped id returned by \code{collabin:::lookup_lope_id()}.
#' @param author_yml_dir String. The path to the directories where
#'   \code{.yml}s of author information are stored.
#'
#' @export
gsheet2post <- function(df,  post_dir_name = NULL,
                        lookup_id = collabin:::lookup_lope_id,
                        author_yml_dir = 'content/author_info/') {
  stopifnot(nrow(df) == 1)

  vars <- c('date', 'id', 'zip_path', 'format',
            'shorturl', 'title', 'subtitle', 'tags')
  gs_yaml <- gs2yaml(df, vars)

  author_id <- lookup_id(gs_yaml$id)

  if (!file.exists(paste0(author_yml_dir, author_id, '.yml'))) {
    stop(author_yml_dir, author_id, '.yml', ' doesn\'t exist.')
  }

  # Construct post yaml header
  post_yaml <- yaml4post(author_id,
                         gs_yaml[c('title','subtitle','tags','date')],
                         author_yml_dir)

  if (is.null(post_dir_name) && is.na(gs_yaml$shorturl)) {
    stop('\nUnknown post directory of ', author_id, '.\n',
         '`shorturl` of the post `', gs_yaml$title ,'` is NA.\n',
         'Please pass the name of post dir to `post_dir_name`.')
  }

  # Get the path to post
  post_dir_name <- ifelse(is.null(post_dir_name),
                     gs_yaml$shorturl,
                     post_dir_name)
  post_dir <- paste('content', author_id, post_dir_name, sep = '/')

  # Attach constructed yaml header to md
  post_file <- ifelse(gs_yaml$format == '.Rmd',
                      '/index.Rmd', '/index.md')
  post_fpath <- paste0(post_dir, post_file)
  # Deal with non-utf8 encodings
  recode_as_utf8(post_fpath)
  attach_yaml2md(post_yaml, md = post_fpath)
}



lookup_lope_id <- function(slack, path = 'content/author_info/id.yml') {
  slack <- normalize(slack)
  lookup_lst <- read_yaml(path)
  author_info <- lookup_lst[slack][[1]]
  return(author_info$id)
}
