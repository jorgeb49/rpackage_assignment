#' Read csv file
#'
#' This is a simple function that, by default, reads the file specified with the 
#' \code{filename} argument.
#'
#' @param filename Name of the csv file that will be read as table data
#'
#' @return This function returns a table using the data in the \code{filename} file
#' the phrase.
#'
#' @note An error will occur if the file does not exist
#'
#' @examples
#' fars_read('accident_2013.csv.bz2')
#'
#' @importFrom readr readcsv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create file name
#'
#' This is a simple function that, takes a string argument, \code{year}, 
#' to create the name of the file that will hold the accidents for that repsectcive year
#'
#' @param year This is the year of the data that will be hold in the file
#'
#' @return This function returns the proper name for the file containing the accidents data 
#' from the year specified as an argument.
#'
#' @note An error will occur if not a  proper year is specicfied as an argument
#'
#' @examples
#' make_filename(2012)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read accident files
#'
#' This is a simple function that, takes an array of years as input, 
#' to read the files that hold the accidents for those repsectcive years
#'
#' @param years This is the years of the data that will be hold in the files
#'
#' @return This function returns the data of the respective years
#'
#' @note An error will occur if a file does not exist for those yerass or the years inout are not valid
#'
#' @examples
#' fars_read_years(c(2013,2014))
#'
#' @importFrom dplyr mutate select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Read and summarize accident files
#'
#' This is a simple function that, takes an array of years as input, 
#' to read the files that hold the accidents for those repsectcive years and summarize the data
#'
#' @param years This is the years of the data that will be summarized
#'
#' @return This function returns the data of the respective years
#'
#' @note An error will occur if a file does not exist for those yerass or the years inout are not valid
#'
#' @examples
#' fars_summarize_years(c(2013,2014))
#'
#' @importFrom dplyr mutate select
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Creates state map with accident of given year
#'
#' This is a function that, takes a sate and set of yeyars as input, 
#' the data is plotted ina a mapof the state for the respective years
#'
#' @param state.num This is a number corresponding to the state of which the map will be created
#' @param years This is the years of the data that will be presented
#'
#' @return This function returns the mapped data of the respective years for the states of interest
#'
#' @note An error will occur if a file does not exist for those yerass or the years inout are not valid 
#' or if the numbers do not correspond to a state
#'
#' @examples
#' fars_map_state(1,2013)
#'
#' @importFrom dplyr mutate select filter
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}


