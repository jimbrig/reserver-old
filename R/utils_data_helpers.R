#' Data Utility Helpers
#'
#' @name data_helpers
#'
#' @description Various data manipulation functions.
#'
#' @return The return value, if any, from executing the utility.
NULL

#' get_states
#'
#' @rdname data_helpers
get_states <- function(dat) {
  unique(dat$state)
}

#' get_accident_years
#'
#' @rdname data_helpers
#' @importFrom dplyr mutate pull
#' @importFrom lubridate year
get_accident_years <- function(dat) {

  dat %>%
    dplyr::mutate(
      year = lubridate::year(accident_date)
    ) %>%
    dplyr::pull("year") %>%
    unique() %>%
    sort()

}

#' show_names
#'
#' @param nms character vector of names from the data
#'
#' @examples
#' show_names(names(trans))
#'
show_names <- function(nms) {
  nms_tbl <- data_frame(data_name = nms)

  nms_tbl <- left_join(nms_tbl, display_names, by = "data_name") %>%
    mutate(display_name = ifelse(is.na(display_name), data_name, display_name))
  nms_tbl$display_name
}


