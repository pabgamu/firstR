#' Load fars data
#'
#' This function reads fars data from *.csv.bz2 file, data is saved as a
#' tibble data frame object.See \code{\link{tbl_df}} for details.
#'
#' @param filename single path to file containing fars data, just filename if
#'        data is stored in the same directory.
#'        If file does not exist function will throw an error.
#'
#'
#' @return a tibble data frame object containing fars data
#'
#' @examples
#' \dontrun{
#' # save file in folder named data in working directory
#' fars_file <- file.path("data", "accident_2014.csv.bz2")
#' fars2012 <- fars_read(fars_file)
#'}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Build file name
#'
#' This function builds file name string of input years. This function does
#' not check whether files actually exist or not. This function assumes files
#' are located in working directory for other functions to work.
#'
#' @param year an integer or numeric vector containing years.
#'
#' @return character vector of the same size containing fars data files names.
#'
#' @examples
#' \dontrun{
#' fars_File_2012 <- make_filename(2012)
#' fars_files_2012_2016 <- make_filename(2012:2016)
#' fars_files_not_conseq <- make_filename(c(2012,2014))
#'}
#'
make_filename <- function(year) {
        year <- as.integer(year)
        file.path('..','inst','extdata', sprintf("accident_%d.csv.bz2", year))
}


#' Extract month and year variables.
#'
#' This function extract month and year data from each file and stores it in
#' a tibble. It throws a warning for any year on which no data file is
#' available.
#'
#' @param years an integer or numeric vector containing years.
#'
#' @return a list of tibbles that include data for each of the input years.
#'
#' @examples
#' \dontrun{
#' # this one return a one element list
#' fars_records_2013 <- fars_read_years(2013)
#'
#' # this one return a two elements list and one warning as 2012 is not available
#' fars_2012_2014 <- fars_read_years(2012:2014)
#'}
#'
#' @importFrom dplyr mutate select %>%
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~ year) %>%
                                dplyr::select_(.dots = c('MONTH', 'year'))
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Number of accidents by month
#'
#' This function computes number of accidents by months for years
#' selected as input. Function will throw a warning for each of the input years
#' that don't have data available.
#' fars data files must be working directory.
#'
#' @param years an integer or numeric vector containing years.
#'
#' @references \url{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @return a tibble data frame object containing number of accidents by
#' month and year.
#'
#' @examples
#' \dontrun{
#' # summarise one year of data
#' monthly_summ_2013 <- fars_summarize_years(2013)
#'
#' # summarise three years of data
#' monthly_summ_2013_2015 <- fars_summarize_years(2013:2015)
#'}
#'
#' @export
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows group_by summarize %>%
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~ year, ~ MONTH) %>%
                dplyr::summarize_(nCount = ~ n()) %>%
                tidyr::spread_(key_col = 'year', value_col = 'nCount')
}


#' Plot accidents location.
#'
#' This function creates a map that has a point in each accident location.
#' fars data files must be in working directory.
#'
#' @param state.num single integer state code in the USA. Function will throw an
#'        error in case no data is available for selected state and year.
#'        In case you input a vector only first element, if data exists, will
#'        be plotted and function will throw a warning.
#'
#' @param year single integer value of the year that will be plotted
#'
#' @return a map object of the selected us state with points in the accident
#'         location of selected year.
#'
#' @seealso \code{\link{map}}
#' @references \url{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @examples
#' \dontrun{
#' # plot existing region
#' fars_map_state(1, 2013)
#'
#' # function throws error as data is not available
#' fars_map_state(2, 2013)
#'
#' # map is plotted but function throws warning
#' fars_map_state(c(1,3), 2013)
#'}
#' @export
#' @importFrom dplyr tbl_df
#' @importFrom graphics points
#' @importFrom maps map
#'
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map(database = "state",
                          ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
