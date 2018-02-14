#' Load single Fatality Analysis Reporting System (FARS) file
#'
#' This function reads a specified file as a tibble format (tbl_df class). The \code{filename}
#' argument should be the path of the file to read.
#'
#' @param filename A file path where the function should look for the FARS data file. .gz, .bz2,
#'    .xz, or .zip files will be automatically uncompressed. Files starting with http://, https://,
#'    ftp://, or ftps:// will be automatically downloaded.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return tibble containing the specified FARS data. If the file is not found at the specified path
#'    the function will return an error. Issues reading in the CSV file also return errors.
#'
#' @examples
#' fars_read("data.csv")
#' fars_read("accident_2013.csv.bz2")
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


#' Create file name for FARS data file
#'
#' This function creates a pathname for FARS data for each specified year.
#'
#' @param year Integer or character string for the year to find data for.
#'
#'
#' @return file path for the specified annual FARS data. If the function is not able to coerce the input
#'    to an integer the function will generate an error.
#'
#' @examples
#' make_filename(2015)
#' make_filename("2013")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data/accident_%d.csv.bz2", year)
}

#' Load FARS data files
#'
#' This function loads FARS files for specified years.
#'
#' @param years Integer or character vector for the years to load data for.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @return separate tibbles for each year. If the function is not able to coerce the inputs
#'    to integers the function will generate an error. If the files are not found at the current working
#'    directory the function will return an error. Issues reading in the CSV files also return errors.
#'
#' @examples
#' fars_read_years(c(2014,2015))
#' fars_read_years("2013")
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      # dplyr::mutate(dat, year = year) %>%
      #   dplyr::select(MONTH, year)
      return(dat)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Load FARS data files and show counts
#'
#' This function loads FARS files for specified years and shows a count of records for each year.
#'
#' @inheritParams fars_read_years
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return table containing data for all specified years and the count of records for each year.
#'    If the function is not able to coerce the inputs to integers the function will generate an error.
#'    If the files are not found at the current working directory the function will return an error.
#'    Issues reading in the CSV files also return errors.
#'
#' @examples
#' fars_summarize_years(c(2014,2015))
#' fars_summarize_years("2013")
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(YEAR, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(YEAR, n)
}

#' Map FARS data reports for a state
#'
#' This function creates a map of fatal vehicle traffic crashes in a specified state and year.
#'
#' @param state.num Integer or character vector for the state FIPS number to map.
#' @param year Integer or character vector for the year to map.
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#'
#' @return map plot showing latitude and longitude locations of traffic fatalities in the specified
#' state and year. If the function is not able to coerce the inputs to integers the function will
#' generate an error. If the files are not found at the current working directory the function
#' will return an error. Issues reading in the CSV files also return errors.
#'
#' @examples
#' fars_map_state(42, 2014)
#' fars_read_years(12, "2013")
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
