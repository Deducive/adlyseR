## Meta (Facebook) ads pull --------------------------------------------------
## Calls fetch_meta_insights() (internal) by default. The insights_fn arg
## remains as an escape hatch for callers who want to plug in an alternative
## (e.g. a facebookadsR wrapper or a stubbed function in tests).

#' Pull Meta (Facebook) ad performance for a campaign
#'
#' @param config       A `campaign_config` object. Reads `meta.account_id`
#'   and `meta.access_token` (preferred) or falls back to the `FB_AD_ACCOUNT`
#'   and `FB_TOKEN` environment variables.
#' @param refresh      Force re-fetch instead of loading from cache.
#' @param insights_fn  Optional override for the function that hits Meta's
#'   API. Defaults to the package-internal [fetch_meta_insights()]. Pass a
#'   different function if you want to stub the API call (e.g. in tests) or
#'   plug in a third-party wrapper.
#'
#' @return A weekly tibble conforming to the canonical channel schema
#'   (see [combine_channels()]). The `platform_conversions` column holds
#'   Meta's pixel-reported conversion count and is NOT comparable to
#'   other channels' or GA4's conversion counts.
#'
#' @details A single API call per campaign is made with `time_increment = "7"`
#'   so Meta returns pre-bucketed weekly rows.
#'
#' @export
pull_meta <- function(config,
                      refresh     = FALSE,
                      insights_fn = NULL) {

  stopifnot(inherits(config, "campaign_config"))

  cached <- archive_load(config, "meta", refresh = refresh)
  if (!is.null(cached)) return(cached)

  ## Default to the package's own fetcher; allow override.
  insights_fn <- insights_fn %||% fetch_meta_insights

  ## Resolve credentials: YAML first, env vars as fallback.
  meta_cfg <- config$meta
  clean <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.character(x)) return(x)
    if (!nzchar(x) || grepl("REPLACE_ME", x, fixed = TRUE)) NULL else x
  }

  fb_account <- clean(meta_cfg$account_id) %||%
    {
      v <- Sys.getenv("FB_AD_ACCOUNT", unset = "")
      if (nzchar(v)) v else NULL
    }
  fb_access_token <- clean(meta_cfg$access_token) %||%
    {
      v <- Sys.getenv("FB_TOKEN", unset = "")
      if (nzchar(v)) v else NULL
    }

  if (is.null(fb_account) || is.null(fb_access_token)) {
    cli::cli_abort(
      c("Meta config incomplete.",
        "i" = "Set {.field meta.account_id} and {.field meta.access_token} in YAML, or define {.envvar FB_AD_ACCOUNT} and {.envvar FB_TOKEN} environment variables.")
    )
  }

  cli::cli_alert_info(
    "Pulling Meta: {config$start_date} \u2192 {config$end_date} (account {fb_account})"
  )

  raw <- tryCatch(
    insights_fn(
      date_from       = config$start_date,
      date_to         = config$end_date,
      time_increment  = "7",
      report_level    = "campaign",
      fb_account      = fb_account,
      fb_access_token = fb_access_token
    ),
    error = function(e) {
      cli::cli_alert_warning("Meta pull failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )

  if (nrow(raw) == 0) {
    out <- empty_channel_weekly("meta")
    archive_save(config, "meta", out)
    return(out)
  }

  raw <- raw %>% filter_by_inventory(config, "meta")

  week_index <- build_week_index(config)

  out <- raw %>%
    dplyr::mutate(event_date = as.Date(.data$date_start)) %>%
    assign_daily_to_weeks(week_index, date_col = "event_date") %>%
    dplyr::group_by(.data$week_num, .data$weeks_to_end, .data$phase,
                    .data$week_start, .data$week_end,
                    .data$campaign_id, .data$campaign_name) %>%
    dplyr::summarise(
      spend                = sum(.data$spend,       na.rm = TRUE),
      impressions          = sum(.data$impressions, na.rm = TRUE),
      clicks               = sum(.data$clicks,      na.rm = TRUE),
      platform_conversions = sum(
        dplyr::coalesce(.data$value_count.purchase, 0),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      channel = "meta",
      cpc = dplyr::if_else(.data$clicks > 0,
                           .data$spend / .data$clicks,
                           NA_real_),
      cpp = dplyr::if_else(.data$platform_conversions > 0,
                           .data$spend / .data$platform_conversions,
                           NA_real_)
    ) %>%
    enforce_channel_schema("meta")

  archive_save(config, "meta", out)
  out
}
