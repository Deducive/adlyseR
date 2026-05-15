## Daily ad-level pulls -------------------------------------------------------

#' @noRd
ad_date <- function(x) as.Date(x)

#' @noRd
clean_optional_secret <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) return(x)
  if (!nzchar(x) || grepl("REPLACE_ME", x, fixed = TRUE)) NULL else x
}

#' @noRd
first_existing_col <- function(df, candidates) {
  candidates[candidates %in% names(df)][1] %||% NA_character_
}

#' @noRd
pull_col <- function(df, candidates, default = NA) {
  hit <- first_existing_col(df, candidates)
  if (is.na(hit)) return(rep(default, nrow(df)))
  df[[hit]]
}

#' @noRd
as_num <- function(x) suppressWarnings(as.numeric(x))

#' @noRd
as_chr <- function(x) as.character(x)

#' @noRd
resolve_meta_credentials <- function(config) {
  meta_cfg <- config$meta
  fb_account <- clean_optional_secret(meta_cfg$account_id) %||%
    {
      v <- Sys.getenv("FB_AD_ACCOUNT", unset = "")
      if (nzchar(v)) v else NULL
    }
  fb_access_token <- clean_optional_secret(meta_cfg$access_token) %||%
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

  list(account_id = fb_account, access_token = fb_access_token)
}

#' Pull Meta daily ad-level performance
#'
#' Pulls daily Meta insights at ad level. Campaign filtering is optional and is
#' applied after the API pull.
#'
#' @param config A `campaign_config` object.
#' @param date_from,date_to Date range to pull.
#' @param campaign_name_groups Optional token groups passed to
#'   [match_campaign_name()].
#'
#' @return A daily ad-level tibble.
#'
#' @export
pull_meta_ads_daily <- function(config,
                                date_from = config$start_date,
                                date_to = min(config$end_date, Sys.Date() - 1),
                                campaign_name_groups = NULL) {
  stopifnot(inherits(config, "campaign_config"))
  creds <- resolve_meta_credentials(config)

  cli::cli_alert_info(
    "Pulling Meta ads daily: {as.Date(date_from)} \u2192 {as.Date(date_to)} (account {creds$account_id})"
  )

  raw <- fetch_meta_insights(
    date_from       = as.Date(date_from),
    date_to         = as.Date(date_to),
    time_increment  = "1",
    report_level    = "ad",
    fb_account      = creds$account_id,
    fb_access_token = creds$access_token
  )

  if (nrow(raw) == 0) {
    return(tibble::tibble())
  }

  df <- raw %>% filter_campaign_names(campaign_name_groups)

  tibble::tibble(
    date = as.Date(pull_col(df, c("date_start"))),
    platform = "meta",
    account_id = creds$account_id,
    campaign_id = as_chr(pull_col(df, c("campaign_id"))),
    campaign_name = as_chr(pull_col(df, c("campaign_name"))),
    ad_group_id = as_chr(pull_col(df, c("adset_id"))),
    ad_group_name = as_chr(pull_col(df, c("adset_name"))),
    ad_id = as_chr(pull_col(df, c("ad_id"))),
    ad_name = as_chr(pull_col(df, c("ad_name"))),
    creative_id = NA_character_,
    creative_name = NA_character_,
    spend = as_num(pull_col(df, c("spend"), 0)),
    impressions = as_num(pull_col(df, c("impressions"), 0)),
    clicks = as_num(pull_col(df, c("clicks"), 0)),
    reach = as_num(pull_col(df, c("reach"), NA_real_)),
    conversions = as_num(
      pull_col(df, c("value_count.purchase", "count.purchase"), 0)
    ),
    conversions_1d_view = as_num(
      pull_col(df, c("1d_view_count.purchase", "count_1d_view.purchase"), 0)
    ),
    conversions_7d_click = as_num(
      pull_col(df, c("7d_click_count.purchase", "count_7d_click.purchase"), 0)
    ),
    conversion_name = "purchase",
    attribution_note = "Meta action_report_time=mixed; windows pulled where available: 1d_view, 7d_click"
  )
}

#' Pull Google Ads daily ad-level performance
#'
#' Pulls daily Google Ads performance from the `ad_group_ad` resource.
#'
#' @inheritParams pull_meta_ads_daily
#'
#' @return A daily ad-level tibble.
#'
#' @export
pull_google_ads_daily <- function(config,
                                  date_from = config$start_date,
                                  date_to = min(config$end_date, Sys.Date() - 1),
                                  campaign_name_groups = NULL) {
  stopifnot(inherits(config, "campaign_config"))

  gads_cfg <- config$google_ads
  customer_id <- gads_cfg$customer_id
  login_customer_id <- gads_cfg$login_customer_id

  if (is.null(customer_id) || !nzchar(customer_id)) {
    cli::cli_abort("Google Ads {.field customer_id} not set in config.")
  }
  if (!requireNamespace("rgoogleads", quietly = TRUE)) {
    cli::cli_abort(
      c("{.pkg rgoogleads} is not installed.",
        "i" = "Run {.code install.packages(\"rgoogleads\")} and try again.")
    )
  }

  cli::cli_alert_info(
    "Pulling Google Ads daily: {as.Date(date_from)} \u2192 {as.Date(date_to)} (customer {customer_id})"
  )

  fields <- c(
    "campaign.id",
    "campaign.name",
    "ad_group.id",
    "ad_group.name",
    "ad_group_ad.ad.id",
    "ad_group_ad.ad.name",
    "ad_group_ad.ad.type",
    "ad_group_ad.status",
    "segments.date",
    "metrics.cost_micros",
    "metrics.impressions",
    "metrics.clicks",
    "metrics.conversions",
    "metrics.all_conversions",
    "metrics.view_through_conversions"
  )

  raw <- tryCatch(
    rgoogleads::gads_get_report(
      resource = "ad_group_ad",
      fields = fields,
      date_from = as.Date(date_from),
      date_to = as.Date(date_to),
      customer_id = customer_id,
      login_customer_id = if (!is.null(login_customer_id) && nzchar(login_customer_id))
        login_customer_id else NULL,
      verbose = FALSE
    ),
    error = function(e) {
      cli::cli_abort(
        c("Google Ads ad-level pull failed: {conditionMessage(e)}",
          "i" = "If this is an auth prompt or token issue, run setup_google_ads() once in an interactive R session.")
      )
    }
  )

  if (nrow(raw) == 0) return(tibble::tibble())

  df <- raw %>% filter_campaign_names(campaign_name_groups)

  tibble::tibble(
    date = as.Date(pull_col(df, c("date", "segments_date"))),
    platform = "google",
    account_id = as_chr(customer_id),
    campaign_id = as_chr(pull_col(df, c("campaign_id"))),
    campaign_name = as_chr(pull_col(df, c("campaign_name"))),
    ad_group_id = as_chr(pull_col(df, c("ad_group_id"))),
    ad_group_name = as_chr(pull_col(df, c("ad_group_name"))),
    ad_id = as_chr(pull_col(df, c("ad_id", "ad_group_ad_ad_id"))),
    ad_name = as_chr(pull_col(df, c("ad_name", "ad_group_ad_ad_name"))),
    creative_id = as_chr(pull_col(df, c("ad_id", "ad_group_ad_ad_id"))),
    creative_name = as_chr(pull_col(df, c("ad_name", "ad_group_ad_ad_name"))),
    spend = as_num(pull_col(df, c("cost", "cost_micros", "metrics_cost_micros"), 0)),
    impressions = as_num(pull_col(df, c("impressions", "metrics_impressions"), 0)),
    clicks = as_num(pull_col(df, c("clicks", "metrics_clicks"), 0)),
    reach = NA_real_,
    conversions = as_num(pull_col(df, c("conversions", "metrics_conversions"), 0)),
    conversions_1d_view = NA_real_,
    conversions_7d_click = NA_real_,
    conversion_name = NA_character_,
    attribution_note = "Google Ads default conversions; attribution windows are configured in Google Ads conversion actions"
  )
}

#' Pull Google Ads daily ad-level conversions by conversion action
#'
#' Pulls Google Ads conversion rows segmented by conversion action name where
#' available. This is a companion table to [pull_google_ads_daily()].
#'
#' @inheritParams pull_meta_ads_daily
#'
#' @return A daily tibble segmented by conversion action.
#'
#' @export
pull_google_ads_conversions_daily <- function(config,
                                              date_from = config$start_date,
                                              date_to = min(config$end_date, Sys.Date() - 1),
                                              campaign_name_groups = NULL) {
  stopifnot(inherits(config, "campaign_config"))

  gads_cfg <- config$google_ads
  customer_id <- gads_cfg$customer_id
  login_customer_id <- gads_cfg$login_customer_id

  if (is.null(customer_id) || !nzchar(customer_id)) {
    cli::cli_abort("Google Ads {.field customer_id} not set in config.")
  }
  if (!requireNamespace("rgoogleads", quietly = TRUE)) {
    cli::cli_abort("{.pkg rgoogleads} is not installed.")
  }

  fields <- c(
    "campaign.id",
    "campaign.name",
    "ad_group.id",
    "ad_group.name",
    "ad_group_ad.ad.id",
    "ad_group_ad.ad.name",
    "segments.date",
    "segments.conversion_action_name",
    "segments.conversion_action_category",
    "metrics.conversions",
    "metrics.all_conversions",
    "metrics.view_through_conversions"
  )

  raw <- tryCatch(
    rgoogleads::gads_get_report(
      resource = "ad_group_ad",
      fields = fields,
      date_from = as.Date(date_from),
      date_to = as.Date(date_to),
      customer_id = customer_id,
      login_customer_id = if (!is.null(login_customer_id) && nzchar(login_customer_id))
        login_customer_id else NULL,
      verbose = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "Google Ads conversion-action pull failed: {conditionMessage(e)}"
      )
      tibble::tibble()
    }
  )

  if (nrow(raw) == 0) return(tibble::tibble())

  df <- raw %>% filter_campaign_names(campaign_name_groups)

  tibble::tibble(
    date = as.Date(pull_col(df, c("date", "segments_date"))),
    platform = "google",
    account_id = as_chr(customer_id),
    campaign_id = as_chr(pull_col(df, c("campaign_id"))),
    campaign_name = as_chr(pull_col(df, c("campaign_name"))),
    ad_group_id = as_chr(pull_col(df, c("ad_group_id"))),
    ad_group_name = as_chr(pull_col(df, c("ad_group_name"))),
    ad_id = as_chr(pull_col(df, c("ad_id", "ad_group_ad_ad_id"))),
    ad_name = as_chr(pull_col(df, c("ad_name", "ad_group_ad_ad_name"))),
    conversion_name = as_chr(pull_col(df, c("conversion_action_name", "segments_conversion_action_name"))),
    conversion_category = as_chr(pull_col(df, c("conversion_action_category", "segments_conversion_action_category"))),
    attribution_event_type = NA_character_,
    conversions = as_num(pull_col(df, c("conversions", "metrics_conversions"), 0)),
    all_conversions = as_num(pull_col(df, c("all_conversions", "metrics_all_conversions"), 0)),
    view_through_conversions = as_num(pull_col(df, c("view_through_conversions", "metrics_view_through_conversions"), 0)),
    attribution_note = "Google Ads conversion action attribution settings; segmented by conversion action where API returns it"
  )
}
