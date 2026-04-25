## Combine channels ----------------------------------------------------------
## Takes the per-channel tibbles from pull_*() and (optionally) the GA4
## traffic tibble, and produces both a long-format spend table and a wide
## weekly summary suitable for plotting or joining further.
##
## Note on conversions: each channel's `platform_conversions` column is a
## platform-reported measurement (Meta pixel, Google Ads, LinkedIn CAPI)
## with channel-specific methodology and cross-device biases. These numbers
## are NOT comparable across channels and MUST NOT be summed into a single
## "total conversions" figure. For cross-channel conversion context we rely
## on GA4 (session-scoped, with its own cross-device undercount caveat) or
## on attributed transaction data joined via `join_spend_and_revenue()`.

#' Combine channel pulls into a unified weekly dataset
#'
#' @param ...    One or more per-channel tibbles (output of `pull_meta()`,
#'   `pull_google_ads()`, `pull_linkedin()`, or any other channel that
#'   conforms to `channel_weekly_schema`).
#' @param ga4    Optional weekly GA4 tibble from `pull_ga4_sessions()` or
#'   `pull_ga4_pagepath()`.
#' @param config A `campaign_config`. If supplied, the returned object gets
#'   its campaign name attached for downstream reporting.
#'
#' @return A list with three named tibbles:
#'   \describe{
#'     \item{spend_long}{One row per (week, channel). Includes
#'       `platform_conversions` which should be used only within-channel.}
#'     \item{weekly_total}{One row per week with cross-channel totals for
#'       spend, impressions, and clicks only (not conversions <e2><80><94> see note
#'       above).}
#'     \item{weekly_combined}{`weekly_total` left-joined to `ga4` and
#'       enriched with `cost_per_session` and `cost_per_ga4_conversion`.}
#'   }
#'
#' @export
combine_channels <- function(..., ga4 = NULL, config = NULL) {

  channels <- list(...)
  channels <- channels[vapply(channels, function(x) nrow(x) > 0, logical(1))]

  if (length(channels) == 0) {
    cli::cli_abort("No non-empty channel tibbles supplied.")
  }

  spend_long <- dplyr::bind_rows(channels) %>%
    dplyr::select(dplyr::all_of(channel_weekly_schema))

  weekly_total <- spend_long %>%
    dplyr::group_by(.data$week_num, .data$weeks_to_end, .data$phase,
                    .data$week_start, .data$week_end) %>%
    dplyr::summarise(
      total_spend       = sum(.data$spend,       na.rm = TRUE),
      total_impressions = sum(.data$impressions, na.rm = TRUE),
      total_clicks      = sum(.data$clicks,      na.rm = TRUE),
      .groups = "drop"
    )

  weekly_combined <- weekly_total
  if (!is.null(ga4)) {
    weekly_combined <- weekly_combined %>%
      dplyr::left_join(
        ga4 %>% dplyr::select(
          .data$week_start,
          ga4_sessions    = .data$sessions,
          ga4_conversions = .data$conversions,
          ga4_conv_rate   = .data$conversion_rate
        ),
        by = "week_start"
      ) %>%
      dplyr::mutate(
        cost_per_session         = dplyr::if_else(
          .data$ga4_sessions > 0,
          .data$total_spend / .data$ga4_sessions,
          NA_real_
        ),
        cost_per_ga4_conversion  = dplyr::if_else(
          .data$ga4_conversions > 0,
          .data$total_spend / .data$ga4_conversions,
          NA_real_
        )
      )
  }

  out <- list(
    spend_long      = spend_long,
    weekly_total    = weekly_total,
    weekly_combined = weekly_combined
  )

  if (!is.null(config)) {
    attr(out, "campaign") <- config$name
  }
  out
}
