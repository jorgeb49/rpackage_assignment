#' Loads a CSV file
#'
#' @description
#' The function loads a CSV file defined by \code{filename} argument and returns
#' a tibble. If the path is incorrect the function will end with an error.
#'
#' @param filename Path to the CSV file (character)
#'
#' @return The function returns a tibble (data.frame) based on the CSV file.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
