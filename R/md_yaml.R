#' Attach yaml to md
#'
#' @param yaml A list or a string. If a list is passed to
#'   \code{yaml}, it is treated as a yaml structure as
#'   returned by \code{yaml::yaml.load()}. If a file path
#'   is passed, \code{yaml::read_yaml()} will be used to
#'   read the file.
#' @param md Path to the \code{.md} or \code{.Rmd} file.
#' @param out Path to the output \code{.md} file. Defaults
#'   to the path specified in \code{md}, which means
#'   overwriting the input file.
#'
#' @export
attach_yaml2md <- function(yaml, md, out = md) {
  stopifnot(is_md(md) || is_rmd(md))

  # Case where `yaml` is local file path
  if (!is.list(yaml) && length(yaml) == 1) {
    stopifnot(is_yaml(yaml))
    yaml <- read_yaml(yaml)
  }

  yaml_body <- blogdown:::split_yaml_body(readLines(md))

  # Deal with non-empty yaml header in md
  if (length(yaml_body$yaml) != 0) {

    # Parse yaml header in md to R obj
    yml_vec <- yaml_body$yaml[2:(length(yaml_body$yaml) - 1)]
    yml_string <- paste(yml_vec, collapse = '\n')
    yaml_in_md <- yaml.load(yml_string)

    # Append yaml options in md that are not specified in `yaml`
    for (i in seq_along(yaml_in_md)) {
      if (! names(yaml_in_md[i]) %in% names(yaml)) {
        yaml <- append(yaml, yaml_in_md[i])
      }
    }
  }

  # Remove trailing `\n`
  yaml <- as.yaml(yaml)
  yaml <- substr(yaml, 1, nchar(yaml) - 1)
  yaml <- gsub("tags: '[", "tags: [", yaml, fixed = TRUE)
  yaml <- gsub("]'", "]", yaml, fixed = TRUE)

  writeLines(c('---', yaml, '---', '', yaml_body$body), con = out)
}


#' Combine Two Sources of yaml Together
#'
#' This function is used to combine a 'static' yaml, saved
#' locally in \code{content/author_info/}, and a 'dynamic' yaml,
#' retrieved and computed from responses collected by Google Form.
#'
#' @param author_id String. The id of the author as specified in
#'   \enumerate{
#'     \item \code{content/author_info/id.yml}
#'     \item The filenames of the files stored in \code{content/author_info/}
#'       (except \code{id.yml}).
#'   }
#'   Note that the id is calculated from the response of the Google Form.
#'   It normalizes the respondent-provided id by 1) conversion to lower case
#'   and 2) replace spaces with \code{_}.
yaml4post <- function(author_id, gs_yaml,
                      author_yml_dir = 'manual-test/author_info/') {

  author_yml_path <- paste0(author_yml_dir, author_id, '.yml')
  author_yaml <- extr_yml(author_yml_path, c('author', 'mysite', 'comment'))

  yaml <- append(gs_yaml, author_yaml)
  return(yaml)
}


#' Convert 1 row of data frame into yaml structure
#'
#' c('時間戳記', '文章標題', '文章附標題', '文章標籤 (逗點分隔，例如 "NLP", "Deep Learning", "R")')
#' @keywords internal
gs2yaml <- function(df, vars = NULL) {
  stopifnot(nrow(df) == 1)

  if (is.null(vars)) {vars <- names(yaml)}
  yaml <- yaml.load(as.yaml(df))
  return(yaml[vars])
}



#' Rename yaml obj (a list) based on input lst.
#' @keywords internal
rename_yaml <- function(yaml, rename_lst = list(oldname = 'newname')) {

  new_name <- vector('character', length(yaml))
  for (i in seq_along(yaml)) {
    idx <- names(rename_lst) == names(yaml[i])
    new_name[i] <- rename_lst[idx][[1]]
  }

  names(yaml) <- new_name
  return(yaml)
}

#' Extract specific variables in a yml file into yaml obj
#' @keywords internal
extr_yml <- function(file = 'manual-test/author_info/joychiang.yml',
                      keys = c('author', 'mysite', 'comment', 'id')) {
  yml <- read_yaml(file)
  return(yml[keys])
}



########## Helpers  ################

normalize <- function(x) tolower(gsub(' ', '_', x, fixed = TRUE))
is_yaml <- function(file) tools::file_ext(file) %in% c('yml', 'yaml')
is_md <- function(file) tools::file_ext(file) %in% c('markdown', 'md')
is_rmd <- function(file) tools::file_ext(file) %in% c('Rmd', 'rmd')
as.yaml <- function(...) yaml::as.yaml(...)
yaml.load <- function(...) yaml::yaml.load(...)
read_yaml <- function(...) yaml::read_yaml(...)
