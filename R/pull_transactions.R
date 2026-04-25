## Transactions pull ----------------------------------------------------------
## Reads campaign-attributed transactional data from a Google Sheet and
## returns a canonical long-format transaction tibble with first-click /
## last-click attribution mapped to adlyseR channel buckets.
##
## Config shape expected in YAML:
##
##   transactions:
##     sheet_id: "1mcUjp-y2J_AEsP3LNfhXIEQEtTs2k__q5sGBKptsIhE"
##     tab_name: "convenience.org purchases all sources"
##     item_ids:
##       - "Retailer2023SOI-26SOI-26SOI"
##       - "RetailerNSD23SOI-26SOI-26SOI"
##       - "Supplier2023SOI-26SOI-26SOI"
##
## Auth is delegated: the caller must have authenticated googlesheets4
## before calling pull_transactions(). In the nacs-attribution repo this
## is done via auth/gsheets_auth.R, which reads service-account
## credentials from the standard NACS path.

#' @noRd
ensure_gsheets <- function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    cli::cli_abort(
      c("{.pkg googlesheets4} is not installed.",
        "i" = "Install with {.code install.packages(\"googlesheets4\")}.")
    )
  }
  if (!googlesheets4::gs4_has_token()) {
    cli::cli_abort(
      c("googlesheets4 is not authenticated.",
        "i" = "Run {.code source(\"auth/gsheets_auth.R\")} or call {.code googlesheets4::gs4_auth()} before {.fn pull_transactions}.")
    )
  }
}

## ---- Channel mapping ------------------------------------------------------

#' Map a raw source/medium string to a canonical adlyseR channel bucket
#'
#' Attribution strings from GA4 come in a mix of formats. This helper reduces
#' them to a small, stable set of channel categories that line up with the
#' paid-media channels adlyseR already knows about <e2><80><94> plus buckets for organic,
#' direct, email, referral, and unattributed traffic.
#'
#' @param x Character vector of `source / medium` strings (e.g.
#'   `"google  /  cpc"`, `"(direct)  /  (none)"`, `"linkedin.com  /  referral"`).
#'
#' @return Character vector of the same length, one of:
#'   `"google"`, `"linkedin"`, `"meta"`, `"organic"`, `"direct"`, `"email"`,
#'   `"referral"`, `"unattributed"`, or `"other"`.
#'
#' @export
map_source_medium_to_channel <- function(x) {

  x_raw <- as.character(x)
  # Normalise whitespace around the "/" that GA4 inserts
  x <- stringr::str_squish(x_raw)
  x <- gsub("\\s*/\\s*", " / ", x)
  x_lower <- tolower(x)

  dplyr::case_when(
    is.na(x_raw) | x_raw == "" | x_raw == "NA"                          ~ "unattributed",
    stringr::str_detect(x_lower, "^\\(not set\\)")                      ~ "unattributed",
    stringr::str_detect(x_lower, "^\\(direct\\)")                       ~ "direct",
    # Paid buckets <e2><80><94> check these BEFORE the generic organic/referral fallbacks
    stringr::str_detect(x_lower, "^google / cpc$|^adwords / ppc$")      ~ "google",
    stringr::str_detect(x_lower, "^bing / (cpc|ppc|paid)")              ~ "google",  # bing paid rolls up to google for ROAS purposes? keep separate if preferred
    stringr::str_detect(x_lower, "^(linkedin|linkedin\\.com)( |/)")     ~ "linkedin",
    stringr::str_detect(x_lower, "^(meta|facebook|fb\\.|l\\.facebook)") ~ "meta",
    # Non-paid categories
    stringr::str_detect(x_lower, " / organic$")                         ~ "organic",
    stringr::str_detect(x_lower, " / email$")                           ~ "email",
    stringr::str_detect(x_lower, " / referral$")                        ~ "referral",
    TRUE                                                                 ~ "other"
  )
}

## ---- pull_transactions() --------------------------------------------------

#' Pull attributed transaction data for a campaign
#'
#' Reads per-transaction sales from a Google Sheet, filters to the campaign's
#' configured `item_ids`, attaches week/phase indices, and maps first- and
#' last-click source/medium strings to canonical channel buckets.
#'
#' @param config  A `campaign_config` object.
#' @param refresh Force re-fetch from the sheet instead of loading from cache.
#'
#' @return A tibble with one row per purchased-item line. Columns:
#'   `event_date, week_num, weeks_to_end, phase, week_start, week_end,`
#'   `transaction_id, item_id, item_name, price, quantity, value,`
#'   `fc_source_medium, fc_campaign, fc_channel,`
#'   `lc_source_medium, lc_campaign, lc_channel`.
#'
#' @details
#' Auth must be established by the caller (e.g. via
#' `source("auth/gsheets_auth.R")`). The function does not attempt to
#' authenticate on its own because auth patterns vary by environment.
#'
#' @export
pull_transactions <- function(config, refresh = FALSE) {

  stopifnot(inherits(config, "campaign_config"))

  cached <- archive_load(config, "transactions", refresh = refresh)
  if (!is.null(cached)) return(cached)

  tx_cfg <- config$transactions
  if (is.null(tx_cfg)) {
    cli::cli_alert_warning(
      "No {.field transactions} block in config. Returning empty tibble."
    )
    return(empty_transaction_tbl())
  }

  sheet_id <- tx_cfg$sheet_id
  tab_name <- tx_cfg$tab_name
  item_ids <- tx_cfg$item_ids %||% character()

  if (is.null(sheet_id) || is.null(tab_name)) {
    cli::cli_abort(
      c("Transactions config incomplete.",
        "i" = "Both {.field sheet_id} and {.field tab_name} are required.")
    )
  }

  ensure_gsheets()

  cli::cli_alert_info(
    "Pulling transactions: tab {.val {tab_name}} ({length(item_ids)} item_id{?s})"
  )

  raw <- tryCatch(
    googlesheets4::read_sheet(sheet_id, sheet = tab_name),
    error = function(e) {
      cli::cli_alert_warning("Transactions read failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )

  if (nrow(raw) == 0) {
    out <- empty_transaction_tbl()
    archive_save(config, "transactions", out)
    return(out)
  }

  ## Filter to configured item_ids
  if (length(item_ids) > 0) {
    if (!"item_id" %in% names(raw)) {
      cli::cli_abort(
        "Transactions tab is missing an {.field item_id} column."
      )
    }
    raw <- raw %>% dplyr::filter(.data$item_id %in% item_ids)
  }

  if (nrow(raw) == 0) {
    cli::cli_alert_warning(
      "No transactions matched configured item_ids."
    )
    out <- empty_transaction_tbl()
    archive_save(config, "transactions", out)
    return(out)
  }

  ## Clean the raw columns. The sheet uses dotted names (fc.source.medium) <e2><80><94>
  ## rename to snake_case for a tidy-friendly output.
  raw <- raw %>%
    dplyr::rename(
      transaction_id    = dplyr::any_of("ecommerce.transaction_id"),
      fc_source_medium  = dplyr::any_of("fc.source.medium"),
      lc_source_medium  = dplyr::any_of("lc.source.medium"),
      fc_campaign       = dplyr::any_of("fc.campaign"),
      lc_campaign       = dplyr::any_of("lc.campaign")
    ) %>%
    dplyr::mutate(
      event_date        = as.Date(.data$event_date),
      price             = as.numeric(.data$price),
      quantity          = as.numeric(.data$quantity),
      value             = as.numeric(.data$value),
      fc_source_medium  = as.character(.data$fc_source_medium),
      lc_source_medium  = as.character(.data$lc_source_medium),
      fc_campaign       = as.character(.data$fc_campaign),
      lc_campaign       = as.character(.data$lc_campaign),
      fc_channel        = map_source_medium_to_channel(.data$fc_source_medium),
      lc_channel        = map_source_medium_to_channel(.data$lc_source_medium)
    )

  ## Attach week / phase indices
  week_index <- build_week_index(config)
  out <- raw %>%
    assign_daily_to_weeks(week_index, date_col = "event_date") %>%
    dplyr::select(
      "event_date",
      "week_num", "weeks_to_end", "phase",
      "week_start", "week_end",
      dplyr::any_of("transaction_id"),
      "item_id",
      dplyr::any_of("item_name"),
      "price", "quantity", "value",
      "fc_source_medium", "fc_campaign", "fc_channel",
      "lc_source_medium", "lc_campaign", "lc_channel"
    )

  archive_save(config, "transactions", out)
  out
}

## ---- Weekly / channel summaries -------------------------------------------

#' @noRd
empty_transaction_tbl <- function() {
  tibble::tibble(
    event_date        = as.Date(character()),
    week_num          = integer(),
    weeks_to_end      = integer(),
    phase             = factor(),
    week_start        = as.Date(character()),
    week_end          = as.Date(character()),
    transaction_id    = character(),
    item_id           = character(),
    item_name         = character(),
    price             = numeric(),
    quantity          = numeric(),
    value             = numeric(),
    fc_source_medium  = character(),
    fc_campaign       = character(),
    fc_channel        = character(),
    lc_source_medium  = character(),
    lc_campaign       = character(),
    lc_channel        = character()
  )
}

#' Summarise transactions by week and attributed channel
#'
#' @param transactions Output of `pull_transactions()`.
#' @param which Which attribution model to use: `"fc"` (first-click, default)
#'   or `"lc"` (last-click).
#'
#' @return A tibble with one row per (week, channel): transaction count,
#'   unit count, attributed revenue (`value`).
#'
#' @export
summarise_transactions_weekly <- function(transactions, which = c("fc", "lc")) {
  which <- match.arg(which)

  if (nrow(transactions) == 0) {
    return(tibble::tibble(
      week_num = integer(), weeks_to_end = integer(), phase = factor(),
      week_start = as.Date(character()), week_end = as.Date(character()),
      channel = character(),
      transactions = integer(), units = numeric(), revenue = numeric()
    ))
  }

  chan_col <- if (which == "fc") "fc_channel" else "lc_channel"

  transactions %>%
    dplyr::group_by(
      .data$week_num, .data$weeks_to_end, .data$phase,
      .data$week_start, .data$week_end,
      channel = .data[[chan_col]]
    ) %>%
    dplyr::summarise(
      transactions = dplyr::n_distinct(.data$transaction_id),
      units        = sum(.data$quantity, na.rm = TRUE),
      revenue      = sum(.data$value,    na.rm = TRUE),
      .groups = "drop"
    )
}

#' Join paid-channel spend against attributed transaction revenue
#'
#' Produces a channel-level view combining adlyseR's `spend_long` with the
#' weekly attributed-revenue tibble. Useful for ROAS and assist/close tables.
#'
#' @param spend_long     Output of `combine_channels()$spend_long`.
#' @param transactions   Output of `pull_transactions()`.
#' @param which          `"fc"` (default) or `"lc"` attribution.
#'
#' @return A tibble per (week, channel) with `spend`, `impressions`, `clicks`,
#'   `transactions`, `units`, `revenue`, and `roas`. Non-paid channels
#'   (organic / direct / email / referral / unattributed / other) appear
#'   with spend = 0; paid channels with no attributed revenue get revenue = 0.
#'
#' @export
join_spend_and_revenue <- function(spend_long, transactions,
                                   which = c("fc", "lc")) {
  which <- match.arg(which)

  spend_weekly <- spend_long %>%
    dplyr::group_by(
      .data$week_num, .data$weeks_to_end, .data$phase,
      .data$week_start, .data$week_end, .data$channel
    ) %>%
    dplyr::summarise(
      spend       = sum(.data$spend,       na.rm = TRUE),
      impressions = sum(.data$impressions, na.rm = TRUE),
      clicks      = sum(.data$clicks,      na.rm = TRUE),
      .groups = "drop"
    )

  revenue_weekly <- summarise_transactions_weekly(transactions, which = which)

  out <- dplyr::full_join(
    spend_weekly, revenue_weekly,
    by = c("week_num", "weeks_to_end", "phase",
           "week_start", "week_end", "channel")
  ) %>%
    dplyr::mutate(
      dplyr::across(
        c(dplyr::any_of(c("spend", "impressions", "clicks",
                          "transactions", "units", "revenue"))),
        ~ dplyr::coalesce(.x, 0)
      ),
      roas = dplyr::if_else(.data$spend > 0, .data$revenue / .data$spend, NA_real_)
    )

  attr(out, "attribution") <- which
  out
}
