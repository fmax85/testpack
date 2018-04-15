#' Data file loader
#'
#' It creates a data file object of 'tbl_df' type from the file that is given
#' by the \code{filename} input parameter. An error is raised if the file name
#' provided does not exist
#'
#' @param filename A string representing a file name
#'
#' @importFrom readr read_csv
#'
#' @return A tbl_df object if the input file exists
#'
#' @examples
#' fars_read("data/accident_2014.csv.bz2")
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

#' File name generator
#'
#' It creates a formatted string needed to load the corresponding data file,
#' provided an appropriate value of the \code{year} input parameter.
#'
#' @param year A numeric value representing the year
#'
#' @return A formatted string representing a data file name that can be used
#'    by \link{fars_read} to load yearly data
#'
#' @examples
#' make_filename(2014)
#'
#' @note Values of the \code{year} parameter which are not of numeric type will
#'    be accepted but might produce inexistent file names!
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Multiple data file loader
#'
#' It creates a list containing the data objects ('tbl_df' type) corresponding
#' to the years provided by the \code{years} input parameter. The data include
#' only the \code{MONTH} and \code{YEAR} fields. NULL is returned for the data
#' table whose year does not exist
#'
#' @param years An array of numeric representing the years referring to the
#'    data to be loaded
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @return A list object containing the data frames referring to years provided
#'    as input, if the corresponding files exist; NULL otherwise
#'
#' @examples
#' fars_read_years( c(2013, 2014) )
#'
#' @note Data files to be loaded must be placed in the same directory of this
#'    R script!
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

#' Data summary generator
#'
#' It creates a 'tbl_df' object containing the summary of the counts of fatal
#' crashes, grouped by month, for any valid year provided in the \code{years}
#' input parameter. An error is generated if no valid year is entered
#'
#' @inheritParams fars_read_years
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @return A table object (of 'tbl_df' type) containing the data summary
#'
#' @examples
#' fars_summarize_years( c(2013, 2018) )
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot of crash locations
#'
#' It creates a map of the desired state with the location of the fatal crashes
#' occurred in the desired year. Data with non-consistent geographical values are
#' filtered out. An error is raised if a inexistent state number is entered
#'
#' @inheritParams make_filename
#'
#' @param state.num A numeric value representing the state ID
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return NULL. An error is raised if an inexistent state number is entered
#'
#' @examples
#' fars_map_state(13, 2015)
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
