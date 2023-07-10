
#'fars_read() Function to read in data from the working directory.
#'@description
#'A simple function to read in  a data file in the form of csv.bz2 using the functionality of the
#'readr::read_csv function. It throws an error, in case the file specified is not existent in the working directory.
#'All test data are included in the package and can be accessed from the US National Highway Traffic Safety Administration's
#'Fatality Analysis Reporting System

#' @param
#' filename as input serves the string of the filename, which is a compressed .csv file
#' @returns
#' Returns a tibble of various attributes of the data. We're only interested in the
#' columns" STATE, MONTH & YEAR, which provide geolocation info about accidents
#' @export
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#'
#'\dontrun{
#'"Throws an error in case of no existent files"
#'   fars_read("accident_2012.csv.bz2")
#'}
#'
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}
#####_________________________________________________________________________#####


#' make_filename(). Help function.
#'
#' @description
#' Help function that can be used in conjunction with the other functions of the package.
#' It takes as input a year as_numeric and returns a text file
#' appropriately composed, so as to conform with the filenames of the compressed files provided
#' @param
#' year as numeric value in the format YYYY, e.g. 2013
#' @returns
#' Returns a filename as text,e.g.:"accident_2013.csv.bz2"
#' @export
#'
#' @examples
#' make_filename(2013)
#'
#' "It can also be used as a helper function, for example in reading in files with `fars_read()` "
#' \code{\link{fars_read(make_filename(2013))}}
#'
#'
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}


#####_________________________________________________________________________#####



#' fars_read_years(). A function to filter data for specific years
#'
#'@description
#' It accepts a vector of numeric values that must correspond to year notation YYYY, e.g. 2013
#' It returns a list of three tibbles, each comprised of two columns, namely: MONTH & year, which themselves are numeric columns
#' it uses the vectorised function to lapply, which is applied on the input vector and retrieves the
#' necessary columns , i.e. Month & year
#'
#' @param
#' years A numeric vector of year values, e.g. c(2013,2014,2015)
#'
#' @returns
#' A list of tibbles of the same length as the length of the input vector
#' @details
#' These functions ARE NOT TO BE USED INTERACTIVELY, because each one of them encapsulates the previous one
#' @export
#'
#' @importFrom dplyr mutate select n
#' @examples
#' fars_read_years(c(2013,2014))
#' \dontrun{
#' "Catches an error in case an invalid year is specified, without stopping the code execution
#' with the implementation of `tryCatch()` and returns a warning invalid year: 2012
#' `fars_read_years(c(2013,2012))`"
#' }

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





#####_________________________________________________________________________#####



#' fars_summarize_years(). A function to summarize particular attributes of the data.
#'
#' @description
#' It accepts a vector of numeric values that must correspond to year notation YYYY, e.g. 2013
#' Row-binds the output of the corresponding tibbles produced from fars_read_years(), groups by year and month
#' and sums up for each combination. Appropriate output is rendered with use of the spread() function
#' of the tidyr package.
#' @param
#' years A numeric vector of year values, e.g. c(2013,2014,2015)
#' @returns
#' A tibble of columns "MONTH" and each one of the years specified in the input vector.
#' @details
#' These functions ARE NOT TO BE USED INTERACTIVELY, because each one of them encapsulates the previous one
#' @export
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}



#####_________________________________________________________________________#####




#' fars_map_state(). A function to draw out points representing geographic locations of accidents.
#'
#'@description
#' It accepts two arguments, state.num, representing an integer corresponding to each one of the 56 States
#' and year, as numeric value in the format YYYY
#' Some data from specific states are not included, specifically of (3,7,14,43,52). If such a case arises
#' a message is returned with the info: "no accidents to plot"
#' Otherwise, in case that geografic coordinates lie within specific boundaries, lon :<900 lat <90
#' a plot is drawn with the graphics::points() function, with each point showing the geografic location of
#' accident in the specific State.
#' Data are filtered for the specific state.num provided with dplyr::filter
#'
#' @param state.num An integer representing each of States of the U.S.
#' @param year  A numeric value in the format YYYY
#'
#' @return
#' Plots a state's layout with points, depicting the geolocation of accidents in the specific area
#' @details
#' These functions ARE NOT TO BE USED INTERACTIVELY, because each one of them encapsulates the previous one
#' @export
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \donttest{fars_map_state(20,2013)}
#'"Plots a map of points representing accident location in the State"
#' \dontrun{
#'
#' "It throws an error in case an invalid state number is provided
#' `fars_map_state(7,2014)`, which will stop execution and return the message `invalid STATE number: .`
#' In the case that no data of accidents are found for the combination of arguments provided
#' a message of `no accidents to plot` and `NULL` is returned
#' `fars_map_state(2,2015)`"
#' }
#'
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
