#' Read file and create a tibble
#'
#' This is a simple function that loads the CSV file that you input, if it exists
#' in the working directory. If the process is successful a tibble will be outputed
#' to the console.
#'
#' @param filename  A character string which contains the name of the csv file
#' that you want to load and exists in the wd.
#'
#' @return This function returns a table with the first ten rows of the csv file
#' In the form of a dplyr tibble.
#'
#' @details  This function contains a control fork (if) that allows the handling
#' of inexistent files or bad spelling of the file's name. In the case that the
#' file doesn't exist the code will stop and a message will appear indicating
#' that the process was not successful.
#'
#' @details This function uses the readr library in order to read rectangular
#' data and the dplyr library to create a tibble (data.frame).
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv")}
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

#' Make a file name
#'
#' @description
#' This function creates a filename for a .csv.bz2 file based on the year that
#' you input, yielding a string with the format "accident_<year>.csv.bz2".
#'
#' @param year Numerical or integer value indicating the year of the data.
#'
#' @return Returns a character string with the format "accident_<year>.csv.bz2".
#'
#' @details It requires a numerical or integer input otherwise ends with an error.
#'
#' @examples
#' \dontrun{makefilename(2016)}
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year from accident files using dplyr
#'
#' @description
#' This function accepts as an input a vector or list of years and returns a list
#' of data frames with MONTH and year columns based on the data loaded from
#' "accident_<year>.csv.bz2" files.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return Returns a list of tibbles with the same number of rows
#' as the "accident_<year>.csv.bz2" files and two columns (MONTH and
#' year).
#'
#'
#' @details The files need to exist in the working directory.
#' Returns NULL and a warning if the file does not exist.
#'
#' @details The function requires the use of the dplyr library.
#'
#' @importFrom dplyr %>% mutate select
#'
#' @examples
#' \dontrun{fars_read_years(2013:2015)}
#' \dontrun{fars_read_years(list(2013, 2015))}
#'
#'@export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(.data$MONTH, .data$year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}
#' Counts number of accidents per month and year
#'
#' This function calculates the number of accidents in the US on a monthly basis,
#' based on the list of years given as a input.
#'
#' @param years A vector or list of numeric or integers, that represents the years.
#'
#' @return Returns a tibble with months as rows and years as columns,
#' which contains the number of accidents. If the conditions are not met
#' it returns a warning for each year that does not exist in the data.
#' Also, it returns an error if the argument is not numeric or integer.
#'
#' @details The accident files need to be in the working directory,
#' the years can be passed as a list or a vector.
#'
#' @details The function requires the use of the dplyr and tidyr library.
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{fars_summarize_years(years = list(2013,2014,2015))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the accidents on the US map
#'
#' The function takes the state number and year as an input, and plots the accidents
#' on the US map.
#'
#' @param state.num Numeric or integer value representing the state of the US
#' as per the FARS data.
#'
#' @param year Numeric or integer value representing the year of the accidents.
#'
#' @return Returns a plot of the US containing the accidents based on the state
#' and year.
#'
#' @details The state number must be an integer or numerical and must exist
#' in the FARS data, otherwise the functions terminates with an error. Also
#' returns an error if the data file for the year input does not exist. Finally,
#' returns an error if the state or year does not exist in the data set.
#'
#' @details The function requires the use of the dplyr, maps and graphics
#' libraries.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom rlang .data
#' @examples
#' \dontrun{fars_map_state(state.num = 1, year = 2013)}
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
