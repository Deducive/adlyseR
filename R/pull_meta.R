## Meta (Facebook) ads pull --------------------------------------------------
## Ported from ns-media-planning/ns_pull_meta.R.
## Uses fb_insights_convs() from the user's existing functions/ library
## (or an equivalent facebookadsR wrapper) with time_increment = "7".

#' Pull Meta (Facebook) ad performance for a campaign
#'
#' @param config       A `campaign_config` object.
#' @param refresh      Force re-fetch instead of loading from cache.
#' @param insights_fn  The pulling function. Defaults to
#'   `fb_insights_convs`, which should be sourced from the user's
#'   `functions/fb_conversions_function.R` before calling this function. In
#'   future we may absorb a reference implementation into the package.
#'
#' @return A weekly tibble conforming to the canonical channel schema
#'   (see [combine_channels()]). The `platform_conversions` column holds
#'   Meta's pixel-reported conversion count and is NOT comparable to
#'   other channels' or GA4's conversion counts.
#'
#' @details This is a faithful port of `ns_pull_meta.R`. A single API call
#'   per campaign is made with `time_increment = "7"` so Meta returns
#'   pre-bucketed weekly rows.
#'
#' @export
pull_meta <- function(config,
                      refresh     = FALSE,
                      insights_fn = NULL) {

  stopifnot(inherits(config, "campaign_config"))

  cached <- archive_load(config, "meta", refresh = refresh)
  if (!is.null(cached)) return(cached)

  insights_fn <- insights_fn %||% tryCatch(
    get("fb_insights_convs", envir = .GlobalEnv, inherits = TRUE),
    error = function(e) NULL
  )

  if (is.null(insights_fn)) {
    cli::cli_abort(
      c("Meta pull requires an insights function.",
        "i" = "Source your existing {.code functions/fb_conversions_function.R} before calling {.fn pull_meta}, or pass {.arg insights_fn} explicitly.")
    )
  }

  meta_cfg <- config$meta
  ## Coerce empty strings and "REPLACE_ME" placeholders to NULL so the
  ## %||% fallback to the globals defined by fb_conversions_function.R
  ## actually fires.
  clean <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.character(x)) return(x)
    if (!nzchar(x) || grepl("REPLACE_ME", x, fixed = TRUE)) NULL else x
  }
  fb_account      <- clean(meta_cfg$account_id)   %||% get0("fb_account",      envir = .GlobalEnv)
  fb_access_token <- clean(meta_cfg$access_token) %||% get0("fb_access_token", envir = .GlobalEnv)

  if (is.null(fb_account) || is.null(fb_access_token)) {
    cli::cli_abort(
      c("Meta config incomplete.",
        "i" = "Set {.field meta.account_id} and {.field meta.access_token} in YAML, or define {.code fb_account} / {.code fb_access_token} in the global env.")
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
