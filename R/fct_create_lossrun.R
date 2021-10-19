#' create_lossrun
#'
#' view losses as of a specific date
#'
#' @param val_date date the valuation date of the loss run.  Claim values from `trans`
#' will be values as of the `val_date`
#' @param trans data frame of claims transactions
#'
#' @return data frame of claims (1 claim per row) valued as of the `val_date`
#'
#' @importFrom dplyr `%>%` filter group_by top_n ungroup mutate arrange
#' @importFrom dplyr filter group_by top_n ungroup mutate arrange desc
create_lossrun <- function(val_date, trans_ = trans) {
  trans_ %>%
    dplyr::filter(transaction_date <= val_date) %>%
    dplyr::group_by(claim_num) %>%
    dplyr::top_n(1, wt = trans_num) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(reported = paid + case) %>%
    dplyr::arrange(dplyr::desc(transaction_date))
}
