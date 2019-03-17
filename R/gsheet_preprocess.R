#' Read % Preprocess Response from Google Sheet
#'
#' @param url URL to Google Sheet. The sheet needs to be published
#'   through web for it to work without authentication.
#' @param preprocess Function. A function passed to preprocess
#'   the data frame returned by \code{googlesheets::gs_read()}.
#'   It takes \code{df} as input and returns a modified \code{df}.
#'
#' @importFrom googlesheets gs_read gs_url
#' @export
#'
#' @example
#' \dontrun{
#' url <- "https://docs.google.com/spreadsheets/d/1LeDRR3iXflUDgDD5WdOjXJfLXCDtdBpetna_R6rXtH0/edit?usp=sharing"
#' df <- read_gs(read_gs)
#' }
read_gs <- function(url, preprocess = collabin:::loper) {
  df <- gs_read(gs_url(url))
  df <- clean_fun(df)
  return(df)
}



#' @importFrom magrittr %>%
#' @importFrom dplyr rename mutate
#' @keywords internal
loper <- function(df) {
  processed_df <- df %>%
    rename(
      slack = `Display name on Slack (e.g. \"shukai\")`,
      date = `時間戳記`,
      title = `文章標題`,
      subtitle = `文章附標題`,
      tags = `文章標籤 (逗點分隔，例如 \"NLP\", \"Deep Learning\", \"R\")`,
      format = `文章格式`,
      first_upload = `是否第一次上傳？`,
      zip_path = `文章上傳 (.zip 或將網址放在 \`url.txt\` 上傳)`,
      shorturl = `標題英文簡寫 (用於產生文章網址, 例如 \"write-in-rmd\")`
    ) %>%
    mutate(slack = normalize(slack),
           date = trimws((gsub('[上下]午.+$', '', date)))) %>%
    mutate(date = format(as.Date(date), "%Y-%m-%d")) %>%
    mutate(subtitle = dplyr::if_else(is.na(subtitle), "", subtitle),
           tags = dplyr::if_else(is.na(tags), "[lope]", tags)
           ) %>%
    mutate(tags = dplyr::if_else(grepl('(lope)|(LOPE)', tags),
                                 tags, gsub(']$', ', LOPE]', tags)
                                 )
           )
}
