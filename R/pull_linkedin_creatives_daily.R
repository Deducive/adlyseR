## LinkedIn daily creative-level pull -----------------------------------------

#' @noRd
run_linkedin_creative_request <- function(scope_param, scope_urns, config,
                                          bearer_token, debug = FALSE,
                                          date_from = config$start_date,
                                          date_to = config$end_date) {
  li_cfg      <- config$linkedin
  api_version <- li_cfg$api_version %||% "202509"
  account_id  <- li_cfg$account_id

  start_d <- as.Date(date_from)
  end_d   <- as.Date(date_to)
  account_urn <- paste0("urn:li:sponsoredAccount:", account_id)

  query_string <- paste0(
    "q=statistics",
    "&pivots=List(CAMPAIGN,CREATIVE)",
    "&timeGranularity=DAILY",
    "&dateRange=(start:(year:", lubridate::year(start_d),
    ",month:",                   lubridate::month(start_d),
    ",day:",                     lubridate::day(start_d),
    "),end:(year:",              lubridate::year(end_d),
    ",month:",                   lubridate::month(end_d),
    ",day:",                     lubridate::day(end_d), "))",
    "&accounts=", build_urn_list(account_urn),
    "&", scope_param, "=", build_urn_list(scope_urns),
    "&fields=",
    paste(
      c(
        "pivotValues",
        "dateRange",
        "clicks",
        "costInLocalCurrency",
        "impressions",
        "externalWebsiteConversions",
        "externalWebsitePostClickConversions",
        "externalWebsitePostViewConversions",
        "totalEngagements"
      ),
      collapse = ","
    )
  )

  url <- paste0(li_url_base(), "/adAnalytics?", query_string)
  if (isTRUE(debug)) cli::cli_alert_info("LinkedIn creative request URL: {url}")

  resp <- httr::GET(
    url,
    httr::add_headers(.headers = li_headers(bearer_token, api_version))
  )
  parsed <- parse_linkedin_content(resp)

  if (isTRUE(debug)) {
    cli::cli_alert_info(
      "LinkedIn creative status {httr::status_code(resp)}; body: {substr(parsed$raw_text, 1, 500)}"
    )
  }

  list(
    status_code = httr::status_code(resp),
    parsed      = parsed$parsed,
    raw_text    = parsed$raw_text,
    url         = url
  )
}

#' @noRd
flatten_linkedin_creative_elements <- function(parsed, scope_label, scope_value) {
  out <- purrr::map_dfr(parsed$elements, function(el) {
    pvs <- el$pivotValues %||% character()
    tibble::tibble(
      campaign_id_lookup = as.character(readr::parse_number(
        as.character(pvs[1] %||% NA_character_)
      )),
      creative_id_lookup = as.character(readr::parse_number(
        as.character(pvs[2] %||% NA_character_)
      )),
      creative_urn = as.character(pvs[2] %||% NA_character_),
      clicks = as.numeric(el$clicks %||% 0),
      spend = as.numeric(el$costInLocalCurrency %||% 0),
      impressions = as.numeric(el$impressions %||% 0),
      externalWebsiteConversions = as.numeric(
        el$externalWebsiteConversions %||% 0
      ),
      externalWebsitePostClickConversions = as.numeric(
        el$externalWebsitePostClickConversions %||% 0
      ),
      externalWebsitePostViewConversions = as.numeric(
        el$externalWebsitePostViewConversions %||% 0
      ),
      totalEngagements = as.numeric(el$totalEngagements %||% 0),
      event_date = lubridate::make_date(
        year  = el$dateRange$start$year  %||% NA_integer_,
        month = el$dateRange$start$month %||% NA_integer_,
        day   = el$dateRange$start$day   %||% NA_integer_
      )
    )
  })

  if (nrow(out) == 0) {
    out$.scope_kind  <- character()
    out$.scope_value <- character()
  } else {
    out$.scope_kind  <- scope_label
    out$.scope_value <- as.character(scope_value)
  }
  out
}

#' @noRd
fetch_li_creative_scope <- function(scope_label, scope_value, config,
                                    bearer_token, debug = FALSE,
                                    date_from = config$start_date,
                                    date_to = config$end_date) {
  scope_param <- switch(
    scope_label,
    campaign_group_id = "campaignGroups",
    campaign_id       = "campaigns",
    cli::cli_abort("Unknown LinkedIn scope label: {.val {scope_label}}")
  )
  urn_prefix <- switch(
    scope_label,
    campaign_group_id = "urn:li:sponsoredCampaignGroup:",
    campaign_id       = "urn:li:sponsoredCampaign:"
  )
  urn <- paste0(urn_prefix, scope_value)

  cli::cli_alert_info(
    "  Pulling LinkedIn creatives for {scope_label}={scope_value}"
  )

  result <- tryCatch(
    run_linkedin_creative_request(scope_param, urn, config, bearer_token,
                                  debug = debug, date_from = date_from,
                                  date_to = date_to),
    error = function(e) {
      cli::cli_alert_warning(
        "LinkedIn creative request failed for {scope_label}={scope_value}: {conditionMessage(e)}"
      )
      NULL
    }
  )

  if (is.null(result)) return(tibble::tibble())

  if (result$status_code >= 300) {
    detail <- result$parsed$message %||%
      result$parsed$serviceErrorCode %||%
      "no error body"
    cli::cli_alert_warning(
      "LinkedIn creative HTTP {result$status_code} for {scope_label}={scope_value}. Detail: {detail}"
    )
    if (!isTRUE(debug)) {
      cli::cli_alert_warning(
        "Raw body starts: {substr(result$raw_text, 1, 300)}"
      )
    }
    return(tibble::tibble())
  }

  elements <- result$parsed$elements
  if (is.null(elements) || length(elements) == 0) {
    cli::cli_alert_warning(
      "LinkedIn returned 0 creative rows for {scope_label}={scope_value}."
    )
    return(tibble::tibble())
  }

  flatten_linkedin_creative_elements(result$parsed, scope_label, scope_value)
}

#' Pull LinkedIn daily creative-level performance
#'
#' Pulls daily LinkedIn reporting at campaign/creative grain. LinkedIn ads map
#' most closely to creatives in the reporting API.
#'
#' @param config A `campaign_config` object.
#' @param date_from,date_to Date range to pull.
#' @param campaign_name_groups Optional token groups passed to
#'   [match_campaign_name()].
#' @param debug If `TRUE`, prints request URLs and response snippets.
#'
#' @return A daily creative-level tibble.
#'
#' @export
pull_linkedin_creatives_daily <- function(config,
                                          date_from = config$start_date,
                                          date_to = min(config$end_date, Sys.Date() - 1),
                                          campaign_name_groups = NULL,
                                          debug = FALSE) {
  stopifnot(inherits(config, "campaign_config"))

  li_cfg <- config$linkedin
  account_id      <- li_cfg$account_id
  campaign_groups <- li_cfg$campaign_groups %||% character()
  campaign_ids    <- li_cfg$campaigns       %||% character()

  if (is.null(account_id) || !nzchar(account_id)) {
    cli::cli_abort("LinkedIn {.field account_id} not set in config.")
  }
  if (is.null(campaign_name_groups) &&
      length(campaign_groups) == 0 && length(campaign_ids) == 0) {
    cli::cli_abort(
      "LinkedIn config has neither {.field campaign_groups} nor {.field campaigns}."
    )
  }

  ensure_linkedin_deps()
  bearer_token <- get_linkedin_access_token(config)
  scope <- resolve_linkedin_scope(config, bearer_token, campaign_name_groups)
  campaign_groups <- scope$campaign_groups
  campaign_ids <- scope$campaign_ids

  if (length(campaign_groups) == 0 && length(campaign_ids) == 0) {
    return(tibble::tibble())
  }

  cli::cli_alert_info(
    "Pulling LinkedIn creatives daily: {as.Date(date_from)} \u2192 {as.Date(date_to)} (account {account_id})"
  )

  raw_groups <- purrr::map_dfr(
    campaign_groups,
    function(g) fetch_li_creative_scope("campaign_group_id", g, config,
                                        bearer_token, debug = debug,
                                        date_from = date_from,
                                        date_to = date_to)
  )
  raw_campaigns <- purrr::map_dfr(
    campaign_ids,
    function(c) fetch_li_creative_scope("campaign_id", c, config,
                                        bearer_token, debug = debug,
                                        date_from = date_from,
                                        date_to = date_to)
  )

  if (nrow(raw_campaigns) > 0 && length(campaign_ids) > 0) {
    requested <- as.character(campaign_ids)
    raw_campaigns <- raw_campaigns %>%
      dplyr::filter(as.character(.data$campaign_id_lookup) %in% requested)
  }

  raw <- dplyr::bind_rows(raw_groups, raw_campaigns)
  if (nrow(raw) == 0) return(tibble::tibble())

  raw <- raw %>%
    dplyr::mutate(
      campaign_id_lookup = as.character(.data$campaign_id_lookup),
      creative_id_lookup = as.character(.data$creative_id_lookup)
    ) %>%
    dplyr::distinct(
      .data$campaign_id_lookup, .data$creative_id_lookup, .data$event_date,
      .keep_all = TRUE
    ) %>%
    dplyr::select(-dplyr::any_of(c(".scope_kind", ".scope_value")))

  campaign_name_tbl <- fetch_linkedin_campaign_names(
    unique(raw$campaign_id_lookup),
    config,
    bearer_token
  )

  raw <- raw %>%
    dplyr::left_join(campaign_name_tbl, by = "campaign_id_lookup") %>%
    filter_campaign_names(campaign_name_groups)

  tibble::tibble(
    date = as.Date(raw$event_date),
    platform = "linkedin",
    account_id = as_chr(account_id),
    campaign_id = as_chr(raw$campaign_id_lookup),
    campaign_name = as_chr(raw$campaign_name),
    ad_group_id = NA_character_,
    ad_group_name = NA_character_,
    ad_id = as_chr(raw$creative_id_lookup),
    ad_name = NA_character_,
    creative_id = as_chr(raw$creative_id_lookup),
    creative_name = NA_character_,
    spend = as_num(raw$spend),
    impressions = as_num(raw$impressions),
    clicks = as_num(raw$clicks),
    reach = NA_real_,
    conversions = as_num(raw$externalWebsiteConversions),
    conversions_1d_view = NA_real_,
    conversions_7d_click = NA_real_,
    externalWebsiteConversions = as_num(raw$externalWebsiteConversions),
    externalWebsitePostClickConversions = as_num(raw$externalWebsitePostClickConversions),
    externalWebsitePostViewConversions = as_num(raw$externalWebsitePostViewConversions),
    conversion_name = NA_character_,
    attribution_note = "LinkedIn externalWebsiteConversions by creative; post-click and post-view fields are included separately"
  )
}
