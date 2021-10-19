#' simulate_claims
#'
#' @description A function to simulate *transactional* actuarial claims/loss
#'   data for Property Casualty Insurance.
#'
#' @param n_claims Numeric - Number of claims to be simulated.
#' @param start_date,end_date Character/Date - Start and End dates for simulation to create claims within (experience_period).
#' @param seed Numeric - the seed is used to isolate randomness during statistical simulations.
#' @param loss_distribution Character - must be one of the distributions mentioned in the details below. Defaults to lognormal.
#' @param probability_open Numeric - must be within `0 < x < 1` and represents probability a claim is open when running binomial simulations for claims' status.
#'
#' @details
#'
#' Severity/Loss Distributions:
#'  - Normal: `norm`
#'  - Lognormal: `lnorm`
#'  - Gamma: `gamma`
#'  - LogGamma: `lgamma`
#'  - Pareto: `pareto`
#'  - Weibull: `weibull`
#'  - Generalized Beta: `genbeta`
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom dplyr mutate arrange bind_rows group_by ungroup filter left_join select
#' @importFrom lubridate days
#' @importFrom randomNames randomNames
#' @importFrom stats rlnorm rnbinom rbinom runif rnorm
#' @importFrom tibble tibble
simulate_claims <- function(n_claims = 1000,
                            start_date = "2015-01-01",
                            end_date = Sys.Date(),
                            seed = 12345,
                            loss_distribution = "lnorm",
                            status_prob_open = 0.96,
                            cache = FALSE,
                            ...) {

  # loss_distribution <- match.arg("loss_distribution")

  stopifnot(
    is.numeric(n_claims) && n_claims > 0,
    class(as.Date(start_date)) == "Date",
    class(as.Date(end_date)) == "Date" &&
      as.Date(end_date) > as.Date(start_date),
    is.numeric(seed),
    loss_distribution %in% c(
      "lnorm",
      "lognormal",
      "normal",
      "gamma",
      "lgamma",
      "pareto",
      "weibull",
      "genbeta"
    ),
    is.numeric(status_prob_open),
    status_prob_open > 0 && status_prob_open < 1
  )

  beg_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  accident_range <- as.numeric(end_date - beg_date)
  set.seed(seed)
  accident_date <- sample(0:accident_range, size = n_claims, replace = TRUE)
  payment_fun <- function(n) stats::rlnorm(n, 7.5, 1.5)

  claims <- tibble::tibble(
    claim_num = paste0("claim-", 1:n_claims),
    accident_date = beg_date + lubridate::days(accident_date),
    state = sample(c("TX", "CA", "GA", "FL"), size = n_claims, replace = TRUE),
    claimant = randomNames::randomNames(n_claims),
    report_lag = stats::rnbinom(n_claims, 5, .25), # 0 if claim closed when reported
    status = stats::rbinom(n_claims, 1, 0.96), # initial payment amount
    payment =  payment_fun(n_claims)
  ) %>%
    dplyr::mutate(
      report_date = accident_date + report_lag,
      payment = ifelse(status == 0, 0, payment),
      case = payment + stats::runif(n_claims, 0.25, 8.0),
      transaction_date = report_date
      ) %>%
    dplyr::arrange(accident_date)

  n_trans <- stats::rnbinom(n_claims, 3, 0.25)
  trans_lag <- lapply(n_trans, function(x) stats::rnbinom(x, 7, 0.1)) %>%
    lapply(function(x) { if (length(x) == 0) 0 else x })

  for (i in seq_len(n_claims)) {
    trans_lag[[i]] <- tibble::tibble(
      "trans_lag" = trans_lag[[i]],
      "claim_num" = paste0("claim-", i)
    )
  }

  trans_tbl <- dplyr::bind_rows(trans_lag) %>%
    dplyr::group_by(.data$claim_num) %>%
    dplyr::mutate(trans_lag = cumsum(trans_lag)) %>%
    dplyr::ungroup()

  # separate all zero claims from the claims that have payments
  zero_claims <- dplyr::filter(claims, status == 0)
  first_trans <- dplyr::filter(claims, status == 1)

  subsequent_trans <- dplyr::left_join(trans_tbl, first_trans, by = "claim_num") %>%
    dplyr::filter(!is.na(.data$accident_date))

  n_trans <- nrow(subsequent_trans)

  subsequent_trans <- subsequent_trans %>%
    dplyr::mutate(payment = payment_fun(n_trans),
           case = pmax(case * stats::rnorm(n_trans, 1.5, 0.1) - payment, 500),
           transaction_date = report_date + trans_lag) %>%
    dplyr::select(-.data$trans_lag)

  trans <- dplyr::bind_rows(zero_claims, first_trans, subsequent_trans) %>%
    dplyr::arrange(transaction_date)

  # add in a transaction number
  trans$trans_num <- 1:nrow(trans)

  # set final trans status to closed and case to 0
  trans <- trans %>%
    dplyr::arrange(trans_num) %>%
    dplyr::group_by(claim_num) %>%
    dplyr::mutate(final_trans = ifelse(trans_num == max(trans_num), TRUE, FALSE),
           status = ifelse(final_trans, 0, 1),
           case = ifelse(final_trans, 0, case),
           status = ifelse(status == 0, "Closed", "Open"),
           paid = round(cumsum(payment), 0),
           case = round(case, 0),
           payment = round(payment, 0)) %>%
    dplyr::select(-final_trans) %>%
    dplyr::arrange(accident_date) %>%
    dplyr::ungroup()

  trans

  if (cache) { saveRDS(trans, file = "trans.RDS") }

  trans

}

