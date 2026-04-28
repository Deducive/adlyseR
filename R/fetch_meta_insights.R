## fetch_meta_insights -------------------------------------------------------
## Internal Meta Marketing API client.
##
## Ported from the standalone fb_insights_convs() function previously sourced
## from `functions/fb_conversions_function.R` in the NACS Show project. Folded
## into the package so pull_meta() no longer depends on caller-supplied code.

#' Fetch Meta (Facebook) ad insights from the Marketing API
#'
#' Calls the Marketing API `/insights` endpoint for an ad account, paginating
#' through the result set, and returns a wide tibble with one row per
#' (campaign or ad, time bucket).
#'
#' Conversion data is reported under Meta's `actions` and `action_values`
#' arrays. This function pivots only the `purchase` action type into wide
#' columns: `count.purchase`, `value.purchase`, `1d_view.purchase`,
#' `7d_click.purchase`, etc. If you need other action types or windows,
#' modify the `purchase` filters below or add post-processing.
#'
#' @param date_from,date_to Date strings (`YYYY-MM-DD`) bounding the report.
#' @param time_increment    Either `"all_days"` (default) or an integer-string
#'   like `"7"` for weekly buckets.
#' @param report_level      One of `"account"`, `"campaign"`, `"adset"`, `"ad"`.
#'   Defaults to `"campaign"`.
#' @param fb_account        The ad account ID (with the `act_` prefix).
#' @param fb_access_token   A Facebook Graph API access token with
#'   `ads_read` scope on the account.
#' @param api_version       Marketing API version, e.g. `"v20.0"`.
#'
#' @return A tibble with character columns for the IDs/names and numeric
#'   columns for everything else, plus parsed `date_start` / `date_stop`.
#'   Empty tibble if Meta returns no rows.
#'
#' @keywords internal
fetch_meta_insights <- function(date_from,
                                date_to,
                                time_increment = "7",
                                report_level   = "campaign",
                                fb_account,
                                fb_access_token,
                                api_version    = "v20.0") {

  if (!requireNamespace("httr", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg httr} is required to pull Meta insights.",
      "i" = "Run {.code install.packages(\"httr\")} and try again."
    ))
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg data.table} is required to pull Meta insights.",
      "i" = "Run {.code install.packages(\"data.table\")} and try again."
    ))
  }

  ## Build the time_range JSON value Meta expects --------------------------
  time_range <- sprintf('{"since":"%s","until":"%s"}', date_from, date_to)

  ## Build endpoint --------------------------------------------------------
  url_stem <- "https://graph.facebook.com/"
  URL <- paste0(url_stem, api_version, "/", fb_account, "/insights")

  ## Columns to keep as character (everything else -> numeric below) -------
  char_cols <- c("date_start", "date_stop",
                 "campaign_name", "campaign_id",
                 "objective", "id",
                 "adset_id", "adset_name",
                 "ad_id", "ad_name")

  fields <- paste(
    "action_values", "actions", "ad_impression_actions", "conversions",
    "campaign_name", "campaign_id", "objective",
    "adset_id", "adset_name", "ad_id", "ad_name",
    "impressions", "cpm", "reach", "frequency",
    "clicks", "unique_clicks", "ctr", "cpc",
    "unique_ctr", "cost_per_unique_click", "spend",
    "canvas_avg_view_time", "canvas_avg_view_percent",
    sep = ", "
  )

  ## First page ------------------------------------------------------------
  first_resp <- httr::GET(
    URL,
    query = list(
      action_attribution_windows = "7d_click,1d_view",
      action_report_time         = "mixed",
      access_token               = fb_access_token,
      action_values              = "7d_click",
      time_range                 = time_range,
      level                      = report_level,
      fields                     = fields,
      time_increment             = time_increment,
      limit                      = "10"
    ),
    encode = "json"
  )
  content_result <- httr::content(first_resp)

  if (!is.null(content_result$error)) {
    cli::cli_abort(c(
      "Meta API error: {content_result$error$message}",
      "i" = "Type: {content_result$error$type}; code: {content_result$error$code}"
    ))
  }

  result_ls <- content_result$data %||% list()

  ## Paginate via paging$next URLs -----------------------------------------
  paging <- content_result$paging
  while (!is.null(paging$`next`)) {
    page <- httr::content(httr::GET(paging$`next`))
    result_ls <- c(result_ls, page$data %||% list())
    paging <- page$paging
  }

  if (length(result_ls) == 0) {
    return(tibble::tibble())
  }

  ## Widen action_values (purchase only, all windows) ----------------------
  result_action_values <- result_ls %>%
    purrr::map(~ .x$action_values) %>%
    purrr::map(dplyr::bind_rows) %>%
    data.table::rbindlist(fill = TRUE, idcol = "id") %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$action_type == "purchase") %>%
    tidyr::pivot_wider(
      id_cols      = "id",
      names_from   = "action_type",
      names_prefix = "amount.",
      values_from  = dplyr::any_of(c("value", "1d_view", "7d_click"))
    )

  ## Widen actions (purchase only, all windows) ----------------------------
  result_actions <- result_ls %>%
    purrr::map(~ .x$actions) %>%
    purrr::map(dplyr::bind_rows) %>%
    data.table::rbindlist(fill = TRUE, idcol = "id") %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$action_type == "purchase") %>%
    tidyr::pivot_wider(
      id_cols      = "id",
      names_from   = "action_type",
      names_prefix = "count.",
      values_from  = dplyr::any_of(c("value", "1d_view", "7d_click"))
    )

  ## Merge values + actions ------------------------------------------------
  result_actions_merge <- merge(result_action_values, result_actions,
                                by = "id", all = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ tidyr::replace_na(as.character(.x), "0"))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  ## Strip action / conversion sublists from the main rows -----------------
  result_no_actions <- result_ls %>%
    purrr::map(~ purrr::discard(.x, stringr::str_detect(names(.x), "action"))) %>%
    purrr::map(~ purrr::discard(.x, stringr::str_detect(names(.x), "conversions"))) %>%
    dplyr::bind_rows(.id = "id")

  ## Final merge -----------------------------------------------------------
  out <- merge(result_no_actions, result_actions_merge,
               by = "id", all.x = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ tidyr::replace_na(.x, "0"))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("date_start", "date_stop")),
                                lubridate::ymd)) %>%
    dplyr::mutate(dplyr::across(c(dplyr::everything(),
                                  -dplyr::any_of(char_cols)),
                                as.numeric)) %>%
    dplyr::select(-"id")

  out
}


#' Pull Meta insights (deprecated alias)
#'
#' Soft-deprecated alias for [fetch_meta_insights()]. Kept so existing
#' projects that still source `functions/fb_conversions_function.R` and pass
#' `insights_fn = fb_insights_convs` continue to work after this function
#' moved into the package. New code should use the package's own
#' [pull_meta()], which calls [fetch_meta_insights()] internally.
#'
#' @inheritParams fetch_meta_insights
#'
#' @keywords internal
#' @export
fb_insights_convs <- function(date_from,
                              date_to,
                              time_increment,
                              report_level,
                              fb_account,
                              fb_access_token) {
  fetch_meta_insights(
    date_from       = date_from,
    date_to         = date_to,
    time_increment  = time_increment,
    report_level    = report_level,
    fb_account      = fb_account,
    fb_access_token = fb_access_token
  )
}
