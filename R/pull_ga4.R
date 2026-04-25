## GA4 pulls -----------------------------------------------------------------
## Ported from ns-media-planning/ns_pull_ga4.R. Two public entry points:
##
##   pull_ga4_sessions() -- all sessions across the site, filtered by
##     hostname in R (matching the original strategy).
##
##   pull_ga4_pagepath() -- sessions restricted to pages matching a path
##     prefix (e.g. "/SOI/"). Useful for campaign-specific landing pages.

## ---- Shared helpers --------------------------------------------------------

#' @noRd
## Ensure googleAnalyticsR is installed AND authenticated.
##
## When `config$ga4$service_account_json` points at a service-account JSON
## file we authenticate via `ga_auth(json_file = ...)`. Otherwise we rely on
## whatever auth the caller has already set up (typically an interactive
## `ga_auth()` that cached a token in .httr-oauth).
ensure_ga4 <- function(config = NULL) {
  if (!requireNamespace("googleAnalyticsR", quietly = TRUE)) {
    cli::cli_abort(
      c("{.pkg googleAnalyticsR} is not installed.",
        "i" = "Install it with {.code install.packages(\"googleAnalyticsR\")} and authenticate with {.code ga_auth()}.")
    )
  }

  if (is.null(config)) return(invisible(NULL))
  svc_json <- config$ga4$service_account_json
  if (is.null(svc_json) || !nzchar(svc_json)) return(invisible(NULL))

  svc_json <- path.expand(svc_json)
  if (!file.exists(svc_json)) {
    cli::cli_alert_warning(
      "GA4 {.field service_account_json} configured but file not found at {.path {svc_json}}. Falling back to existing ga_auth() state."
    )
    return(invisible(NULL))
  }

  tryCatch(
    {
      googleAnalyticsR::ga_auth(json_file = svc_json)
      cli::cli_alert_info(
        "GA4 authenticated via service account {.path {svc_json}}."
      )
    },
    error = function(e) {
      cli::cli_alert_warning(
        "GA4 service-account auth failed: {conditionMessage(e)}. Falling back to existing ga_auth() state."
      )
    }
  )
  invisible(NULL)
}

#' @noRd
## Summarise a daily tibble to weekly and add conversion_rate.
summarise_ga4_weekly <- function(daily_df, week_index) {

  if (nrow(daily_df) == 0) {
    return(tibble::tibble(
      week_num = integer(), weeks_to_end = integer(),
      phase = factor(),
      week_start = as.Date(character()), week_end = as.Date(character()),
      sessions = numeric(), conversions = numeric(), conversion_rate = numeric()
    ))
  }

  daily_df %>%
    assign_daily_to_weeks(week_index, date_col = "event_date") %>%
    dplyr::group_by(.data$week_num, .data$weeks_to_end, .data$phase,
                    .data$week_start, .data$week_end) %>%
    dplyr::summarise(
      sessions    = sum(.data$sessions,    na.rm = TRUE),
      conversions = sum(.data$conversions, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(
      conversion_rate = dplyr::if_else(
        .data$sessions > 0, .data$conversions / .data$sessions, NA_real_
      )
    )
}

## ---- Whole-site sessions (with hostname filter) ---------------------------

#' Pull GA4 sessions for a campaign (whole-site, hostname-filtered)
#'
#' @param config  A `campaign_config` object with `ga4.property_id` and
#'   (optionally) `ga4.hostnames` to include, and (optionally)
#'   `ga4.service_account_json` pointing at a service-account key.
#' @param refresh Force re-fetch.
#'
#' @return A weekly tibble: week_num, weeks_to_end, phase, week_start,
#'   week_end, sessions, conversions, conversion_rate.
#'
#' @export
pull_ga4_sessions <- function(config, refresh = FALSE) {

  stopifnot(inherits(config, "campaign_config"))
  cached <- archive_load(config, "ga4_sessions", refresh = refresh)
  if (!is.null(cached)) return(cached)

  ensure_ga4(config)
  ga4_cfg <- config$ga4
  property <- ga4_cfg$property_id
  if (is.null(property)) cli::cli_abort("Missing {.field ga4.property_id} in config.")

  hostnames <- ga4_cfg$hostnames

  cli::cli_alert_info(
    "Pulling GA4 sessions: {config$start_date} \u2192 {config$end_date} (property {property})"
  )

  daily <- tryCatch(
    googleAnalyticsR::ga_data(
      propertyId = property,
      date_range = c(config$start_date, config$end_date),
      metrics    = c("sessions", "conversions"),
      dimensions = c("date", "hostname"),
      limit      = 100000
    ),
    error = function(e) {
      cli::cli_alert_warning("GA4 sessions fetch failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )

  if (nrow(daily) == 0) {
    out <- summarise_ga4_weekly(daily, build_week_index(config))
    archive_save(config, "ga4_sessions", out)
    return(out)
  }

  if (!is.null(hostnames) && length(hostnames) > 0) {
    daily <- daily %>% dplyr::filter(.data$hostname %in% hostnames)
  }

  daily <- daily %>%
    dplyr::mutate(event_date = as.Date(.data$date)) %>%
    dplyr::group_by(.data$event_date) %>%
    dplyr::summarise(
      sessions    = sum(.data$sessions,    na.rm = TRUE),
      conversions = sum(.data$conversions, na.rm = TRUE),
      .groups     = "drop"
    )

  out <- summarise_ga4_weekly(daily, build_week_index(config))
  archive_save(config, "ga4_sessions", out)
  out
}

## ---- Landing-page sessions (page path filter) ----------------------------

#' Pull GA4 sessions for a specific page-path prefix
#'
#' Pulls daily sessions where `pagePath` starts with `config$ga4$page_path_prefix`
#' (e.g. `"/SOI/"`). Hostname is filtered in R after the pull (the API-level
#' filter on hostname alongside pagePath is unreliable).
#'
#' @param config  A `campaign_config` object with `ga4.property_id`,
#'   `ga4.page_path_prefix`, (optionally) `ga4.hostnames`, and (optionally)
#'   `ga4.service_account_json` for service-account auth.
#' @param refresh Force re-fetch.
#'
#' @return Weekly tibble: week_num, weeks_to_end, phase, week_start, week_end,
#'   sessions, conversions, conversion_rate.
#'
#' @export
pull_ga4_pagepath <- function(config, refresh = FALSE) {

  stopifnot(inherits(config, "campaign_config"))
  cached <- archive_load(config, "ga4_pagepath", refresh = refresh)
  if (!is.null(cached)) return(cached)

  ensure_ga4(config)
  ga4_cfg <- config$ga4
  property <- ga4_cfg$property_id
  path_prefix <- ga4_cfg$page_path_prefix

  if (is.null(property))    cli::cli_abort("Missing {.field ga4.property_id} in config.")
  if (is.null(path_prefix)) cli::cli_abort("Missing {.field ga4.page_path_prefix} in config.")

  hostnames <- ga4_cfg$hostnames

  cli::cli_alert_info(
    "Pulling GA4 page traffic: path prefix {.val {path_prefix}}, {config$start_date} \u2192 {config$end_date}"
  )

  ## ga_data_filter uses a DSL: `field %begins% value`. Using "pagePath" as
  ## a string avoids needing a ga_meta() call up front to validate the field.
  page_filter <- googleAnalyticsR::ga_data_filter("pagePath" %begins% path_prefix)

  daily <- tryCatch(
    googleAnalyticsR::ga_data(
      propertyId  = property,
      date_range  = c(config$start_date, config$end_date),
      metrics     = c("sessions", "conversions"),
      dimensions  = c("date", "pagePath", "hostname"),
      dim_filters = page_filter,
      limit       = 100000
    ),
    error = function(e) {
      cli::cli_alert_warning("GA4 page-path fetch failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )

  if (nrow(daily) == 0) {
    out <- summarise_ga4_weekly(daily, build_week_index(config))
    archive_save(config, "ga4_pagepath", out)
    return(out)
  }

  if (!is.null(hostnames) && length(hostnames) > 0) {
    daily <- daily %>% dplyr::filter(.data$hostname %in% hostnames)
  }

  daily <- daily %>%
    dplyr::mutate(event_date = as.Date(.data$date)) %>%
    dplyr::group_by(.data$event_date) %>%
    dplyr::summarise(
      sessions    = sum(.data$sessions,    na.rm = TRUE),
      conversions = sum(.data$conversions, na.rm = TRUE),
      .groups     = "drop"
    )

  out <- summarise_ga4_weekly(daily, build_week_index(config))
  archive_save(config, "ga4_pagepath", out)
  out
}
