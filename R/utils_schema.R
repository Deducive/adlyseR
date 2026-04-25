## Canonical Channel Schema --------------------------------------------------
## Every pull_meta(), pull_google_ads(), pull_linkedin() returns a tibble
## that conforms to the schema below. This lets combine_channels() just
## bind_rows() them without per-channel logic.
##
## Note on the `platform_conversions` column: each platform (Meta pixel,
## Google Ads, LinkedIn CAPI) measures conversions using its own methodology
## (cookie/device-based, server-side, user-graph, etc.) and these numbers are
## NOT comparable to each other and should NEVER be summed across channels.
## They also typically differ from GA4's on-site conversion count, which is
## reported separately via the GA4 pulls.

#' @noRd
channel_weekly_schema <- c(
  "week_num",
  "weeks_to_end",
  "phase",
  "week_start",
  "week_end",
  "channel",
  "campaign_id",
  "campaign_name",
  "spend",
  "impressions",
  "clicks",
  "platform_conversions",
  "cpc",
  "cpp"
)

#' @noRd
## Return a zero-row tibble matching the canonical schema. The `channel`
## argument sets the factor level / character value but does NOT add a row.
empty_channel_weekly <- function(channel = NA_character_) {
  tibble::tibble(
    week_num             = integer(),
    weeks_to_end         = integer(),
    phase                = factor(),
    week_start           = as.Date(character()),
    week_end             = as.Date(character()),
    channel              = character(),
    campaign_id          = character(),
    campaign_name        = character(),
    spend                = numeric(),
    impressions          = numeric(),
    clicks               = numeric(),
    platform_conversions = numeric(),
    cpc                  = numeric(),
    cpp                  = numeric()
  )
}

#' @noRd
## Validate that a channel pull returned a tibble conforming to schema.
## Used inside each pull_*() just before return.
enforce_channel_schema <- function(df, channel) {

  if (nrow(df) == 0) {
    return(empty_channel_weekly(channel))
  }

  if (!"channel" %in% names(df)) {
    df$channel <- channel
  }

  missing <- setdiff(channel_weekly_schema, names(df))
  for (col in missing) {
    df[[col]] <- switch(
      col,
      week_num      = NA_integer_,
      weeks_to_end  = NA_integer_,
      phase         = factor(NA, levels = levels(df$phase)),
      week_start    = as.Date(NA),
      week_end      = as.Date(NA),
      campaign_id   = NA_character_,
      campaign_name = NA_character_,
      NA_real_
    )
  }

  df[, channel_weekly_schema]
}
