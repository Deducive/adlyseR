## Google Ads pull -----------------------------------------------------------
## Ported from ns-media-planning/ns_pull_google_ads.R. Uses the rgoogleads
## package; caller is expected to have run gads_auth() interactively once to
## establish OAuth.

#' Pull Google Ads performance for a campaign
#'
#' @param config  A `campaign_config` object. Must have
#'   `google_ads.customer_id` set; optionally `google_ads.login_customer_id`.
#' @param refresh Force re-fetch instead of loading from cache.
#'
#' @return A weekly tibble conforming to the canonical channel schema. The
#'   `platform_conversions` column holds Google Ads' reported conversion
#'   count and is NOT comparable to other channels' or GA4's conversion
#'   counts.
#'
#' @export
pull_google_ads <- function(config, refresh = FALSE) {

  stopifnot(inherits(config, "campaign_config"))

  cached <- archive_load(config, "google_ads", refresh = refresh)
  if (!is.null(cached)) return(cached)

  gads_cfg <- config$google_ads
  customer_id       <- gads_cfg$customer_id
  login_customer_id <- gads_cfg$login_customer_id

  if (is.null(customer_id) || !nzchar(customer_id)) {
    cli::cli_alert_warning(
      "Google Ads {.field customer_id} not set in config. Returning empty tibble."
    )
    out <- empty_channel_weekly("google")
    archive_save(config, "google_ads", out)
    return(out)
  }

  if (!requireNamespace("rgoogleads", quietly = TRUE)) {
    cli::cli_abort(
      c("{.pkg rgoogleads} is not installed.",
        "i" = "Run {.code install.packages(\"rgoogleads\")} and try again.")
    )
  }

  cli::cli_alert_info(
    "Pulling Google Ads: {config$start_date} \u2192 {config$end_date} (customer {customer_id})"
  )

  # The user is expected to have already run gads_auth() interactively.
  daily <- tryCatch(
    rgoogleads::gads_get_report(
      resource  = "campaign",
      fields    = c(
        "campaign.id",
        "campaign.name",
        "segments.date",
        "metrics.cost_micros",
        "metrics.clicks",
        "metrics.impressions",
        "metrics.conversions"
      ),
      date_from = config$start_date,
      date_to   = config$end_date,
      customer_id = customer_id,
      login_customer_id = if (!is.null(login_customer_id) && nzchar(login_customer_id))
        login_customer_id else NULL,
      verbose   = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning("Google Ads pull failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )

  if (nrow(daily) == 0) {
    out <- empty_channel_weekly("google")
    archive_save(config, "google_ads", out)
    return(out)
  }

  daily <- daily %>%
    dplyr::transmute(
      event_date            = as.Date(.data$date),
      campaign_id           = as.character(.data$campaign_id),
      campaign_name         = .data$campaign_name,
      spend                 = .data$cost,
      clicks                = .data$clicks,
      impressions           = .data$impressions,
      platform_conversions  = .data$conversions
    ) %>%
    filter_by_inventory(config, "google")

  week_index <- build_week_index(config)

  out <- daily %>%
    assign_daily_to_weeks(week_index, date_col = "event_date") %>%
    dplyr::group_by(.data$week_num, .data$weeks_to_end, .data$phase,
                    .data$week_start, .data$week_end,
                    .data$campaign_id, .data$campaign_name) %>%
    dplyr::summarise(
      spend                = sum(.data$spend,                na.rm = TRUE),
      clicks               = sum(.data$clicks,               na.rm = TRUE),
      impressions          = sum(.data$impressions,          na.rm = TRUE),
      platform_conversions = sum(.data$platform_conversions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      channel = "google",
      cpc = dplyr::if_else(.data$clicks > 0,
                           .data$spend / .data$clicks,
                           NA_real_),
      cpp = dplyr::if_else(.data$platform_conversions > 0,
                           .data$spend / .data$platform_conversions,
                           NA_real_)
    ) %>%
    enforce_channel_schema("google")

  archive_save(config, "google_ads", out)
  out
}
