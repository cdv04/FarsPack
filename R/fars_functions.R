

#' Reading Fatality Analysis Reporting System data
#'
#' This function reads a FARS dataset containig in csv file given in argument. If the file doesn't exist
#' the function stops and prints an arror message saying that the data doesn't exist.
#' If the data exist, the functions reads it and create a data frame from it.
#'
#'
#' @param filename A character string giving the name of the file to read.
#'
#' @return a data frame.
#' @examples
#'filename <- system.file("extdata", make_filename(2014), package = "FarsPack")
#' acc2014 <- fars_read (filename)
#'
#' @details
#' If the format of the file is not csv.bz2 or csv, the function returns an error message saying "invalid file argument"
#'
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






#' Returning a a FARS filename according a year entered as a parameter
#'
#' This function return a character vector containing a formatted combination of text and year value
#'
#' @param year An integer
#'
#' @return a character string
#' @examples
#'
#' name_2017 <- make_filename(2017)
#' name_2017
#'
#' @details
#' the file names created is in csv.bz2 format
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}




#' Creating a list of several FARS dataset
#'
#' This function creates a list of truncated FARS datatsets giving th considered years into a list.
#' Returned FARS datasets are truncated because they only conatines the variables year and month
#'
#'
#' @param years A integer vector of year
#'
#' @return a list of data frame tbl corresponding to the list of years entered in parameters
#' @examples
#'
#' dat_list <- fars_read_years(c(2013,2014,2015))
#'
#' @details
#'if a unknown year is given into the list of year, an error message is printed
#'indicating that the year is invalid. And so the corresponding element of the returned list is NULL
#'
#' @importFrom magrittr %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select_(~MONTH, ~year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}



#' Summarize FARS crash data by year and month
#'
#' @param years Integer vector specifying year(s) to summarize.
#'
#' @return A data frame with counts of fatal crashes by year and month.
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(.dots = list(n = ~ n())) %>%
    tidyr::spread_("year", "n")
}



#' Showing where accidents occured
#'
#' This function a graphic showing in a map where the accident occured given a state number and a year
#' @param state.num A integer
#' @param year A integer
#'
#' @return NULL
#'
#' @examples
#' library(maps)
#' MyMap<- fars_map_state(1,2013)
#'
#' @details
#'If the state number is wrong (>56) an error message is printed.
#'If the state considered contains no accident, a message is printed saying
#'there is no accident to plot
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- system.file("extdata", make_filename(year), package = "FarsPack")
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, .dots = list(~ STATE == state.num))
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

