## LinkedIn ad pull ----------------------------------------------------------
## Ported from nacs-attribution/ns-media-planning/ns_pull_linkedin.R.
##
## LinkedIn Marketing API hierarchy:
##   Account -> Campaign Group -> Campaign -> Creative
##
## A adlyseR campaign can be scoped at EITHER level:
##   * `linkedin$campaign_groups`: vector of group IDs (e.g. "688096446"). Pulls
##     every campaign inside each group. Best when all the campaign's ads live
##     together under one group.
##   * `linkedin$campaigns`: vector of campaign IDs (e.g. "801638263"). Pulls
##     only those specific campaigns. Best when the campaign spans campaigns
##     scattered across different groups.
##
## Note: LinkedIn's adAnalytics endpoint requires `accounts=List(...)` when
## scoping by `campaigns=` (account is always sent). Empirically, LinkedIn
## sometimes returns rows for *all* campaigns in the account even when the
## `campaigns=` filter is specified, so we also filter in memory against the
## requested campaign IDs before returning.

## ---- Helpers: auth ---------------------------------------------------------

#' @noRd
parse_token_expiry <- function(x) {
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
  if (is.na(parsed)) {
    numeric_candidate <- suppressWarnings(as.numeric(x))
    if (!is.na(numeric_candidate)) {
      parsed <- lubridate::as_datetime(numeric_candidate, tz = "UTC")
    }
  }
  parsed
}

#' @noRd
get_linkedin_access_token <- function(config) {

  li_cfg <- config$linkedin
  token_file     <- li_cfg$token_file
  refresh_script <- li_cfg$token_refresh_script

  if (!is.null(token_file) && file.exists(token_file)) {
    li_tokens <- readr::read_csv(token_file, show_col_types = FALSE) %>%
      dplyr::mutate(
        expiry_parsed = purrr::map(.data$expiry, parse_token_expiry) %>%
          purrr::list_c()
      )

    access_row <- li_tokens %>%
      dplyr::filter(.data$token.type == "access_token") %>%
      dplyr::slice(1)

    if (nrow(access_row) == 1 &&
        !is.na(access_row$expiry_parsed[[1]]) &&
        access_row$expiry_parsed[[1]] > lubridate::now(tzone = "UTC")) {
      cli::cli_alert_info(
        "Using cached LinkedIn access token valid until {format(access_row$expiry_parsed[[1]], tz = 'UTC', usetz = TRUE)}"
      )
      return(access_row$token[[1]])
    }
  }

  if (!is.null(refresh_script) && file.exists(refresh_script)) {
    cli::cli_alert_info(
      "Cached LinkedIn token unavailable or expired; sourcing refresh script."
    )
    auth_env <- new.env(parent = globalenv())
    sys.source(refresh_script, envir = auth_env)
    if (exists("a.token", envir = auth_env, inherits = FALSE)) {
      return(get("a.token", envir = auth_env))
    }
    cli::cli_abort(
      "Refresh script {.path {refresh_script}} did not define {.code a.token}."
    )
  }

  cli::cli_abort(
    c("No valid LinkedIn access token found.",
      "i" = "Set {.field linkedin.token_file} (a CSV) or {.field linkedin.token_refresh_script} in your campaign config.")
  )
}

## ---- Helpers: HTTP ---------------------------------------------------------

#' @noRd
li_url_base <- function() "https://api.linkedin.com/rest"

#' @noRd
li_headers <- function(bearer_token, api_version) {
  c(
    Authorization               = paste("Bearer", bearer_token),
    `Linkedin-Version`          = api_version,
    `X-Restli-Protocol-Version` = "2.0.0",
    `Content-Type`              = "application/json"
  )
}

#' @noRd
parse_linkedin_content <- function(resp) {
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(txt)) {
    return(list(parsed = list(), raw_text = txt))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) list(parse_error = conditionMessage(e), raw_text = txt)
  )
  list(parsed = parsed, raw_text = txt)
}

#' @noRd
ensure_linkedin_deps <- function() {
  for (pkg in c("httr", "jsonlite", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(
        c("LinkedIn pull requires {.pkg {pkg}} but it's not installed.",
          "i" = "Run {.code install.packages(\"{pkg}\")} and try again.")
      )
    }
  }
}

## ---- Helpers: campaign name lookup ----------------------------------------

#' @noRd
fetch_linkedin_campaign_names <- function(campaign_ids, config, bearer_token) {

  campaign_ids <- campaign_ids[!is.na(campaign_ids) & nzchar(campaign_ids)]
  if (length(campaign_ids) == 0) {
    return(tibble::tibble(campaign_id_lookup = character(), campaign_name = character()))
  }

  li_cfg      <- config$linkedin
  account_id  <- li_cfg$account_id
  api_version <- li_cfg$api_version %||% "202509"

  campaign_names <- tibble::tibble(
    campaign_id_lookup = character(),
    campaign_name      = character()
  )
  next_page_token <- NULL

  repeat {
    query_string <- paste0(
      "q=search",
      "&pageSize=200",
      if (!is.null(next_page_token)) {
        paste0("&pageToken=", utils::URLencode(next_page_token, reserved = TRUE))
      } else ""
    )

    url <- paste0(
      li_url_base(),
      "/adAccounts/", account_id,
      "/adCampaigns?", query_string
    )

    resp <- httr::GET(
      url,
      httr::add_headers(.headers = li_headers(bearer_token, api_version))
    )
    parsed <- parse_linkedin_content(resp)

    if (httr::status_code(resp) >= 300 || is.null(parsed$parsed$elements)) {
      cli::cli_alert_warning(
        "LinkedIn campaign-name lookup failed. Raw body starts: {substr(parsed$raw_text, 1, 200)}"
      )
      return(tibble::tibble(
        campaign_id_lookup = character(),
        campaign_name      = character()
      ))
    }

    page_names <- parsed$parsed$elements %>%
      purrr::map_dfr(function(el) {
        tibble::tibble(
          campaign_id_lookup = as.character(el$id   %||% NA_character_),
          campaign_name      = as.character(el$name %||% NA_character_)
        )
      }) %>%
      dplyr::filter(!is.na(.data$campaign_id_lookup),
                    !is.na(.data$campaign_name))

    campaign_names <- dplyr::bind_rows(campaign_names, page_names) %>%
      dplyr::distinct()

    if (all(campaign_ids %in% campaign_names$campaign_id_lookup)) break

    next_page_token <- parsed$parsed$metadata$nextPageToken %||% NULL
    if (is.null(next_page_token)) break
  }

  campaign_names %>%
    dplyr::filter(.data$campaign_id_lookup %in% campaign_ids)
}

## ---- Helpers: analytics request ------------------------------------------

#' @noRd
## Build a LinkedIn List() URL param from a vector of URNs.
build_urn_list <- function(urns) {
  encoded <- vapply(urns, utils::URLencode, character(1), reserved = TRUE)
  paste0("List(", paste(encoded, collapse = ","), ")")
}

#' @noRd
## Build and execute one adAnalytics request. scope_param is "campaignGroups"
## or "campaigns"; scope_urns are the URN(s). Always includes
## accounts=List(...) because the analytics finder requires it when scoping
## by campaign (and it's harmless for group scopes).
run_linkedin_request <- function(scope_param, scope_urns, config, bearer_token,
                                 debug = FALSE) {

  li_cfg      <- config$linkedin
  api_version <- li_cfg$api_version %||% "202509"
  account_id  <- li_cfg$account_id

  start_d <- config$start_date
  end_d   <- config$end_date

  account_urn <- paste0("urn:li:sponsoredAccount:", account_id)

  query_string <- paste0(
    "q=analytics",
    "&pivot=CAMPAIGN",
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
        "clicks",
        "costInLocalCurrency",
        "impressions",
        "externalWebsiteConversions",
        "dateRange",
        "externalWebsitePostClickConversions",
        "externalWebsitePostViewConversions",
        "totalEngagements"
      ),
      collapse = ","
    )
  )

  url <- paste0(li_url_base(), "/adAnalytics?", query_string)
  if (isTRUE(debug)) cli::cli_alert_info("LinkedIn request URL: {url}")

  resp <- httr::GET(
    url,
    httr::add_headers(.headers = li_headers(bearer_token, api_version))
  )
  parsed <- parse_linkedin_content(resp)

  if (isTRUE(debug)) {
    cli::cli_alert_info(
      "LinkedIn status {httr::status_code(resp)}; body: {substr(parsed$raw_text, 1, 500)}"
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
## Flatten LinkedIn's nested `elements` array into a daily tibble.
## Columns `.scope_kind` / `.scope_value` record how we fetched the row (group
## vs campaign scope) <e2><80><94> leading-dot names keep them out of the canonical
## schema and avoid clashing with user-facing columns like `campaign_id`.
flatten_linkedin_elements <- function(parsed, scope_label, scope_value) {

  out <- purrr::map_dfr(parsed$elements, function(el) {
    pvs <- el$pivotValues %||% character()
    tibble::tibble(
      campaign_id_lookup = readr::parse_number(
        as.character(pvs[1] %||% NA_character_)
      ),
      clicks                                = as.numeric(el$clicks %||% 0),
      costInLocalCurrency                   = as.numeric(el$costInLocalCurrency %||% 0),
      impressions                           = as.numeric(el$impressions %||% 0),
      externalWebsiteConversions            = as.numeric(el$externalWebsiteConversions %||% 0),
      externalWebsitePostClickConversions   = as.numeric(el$externalWebsitePostClickConversions %||% 0),
      externalWebsitePostViewConversions    = as.numeric(el$externalWebsitePostViewConversions %||% 0),
      totalEngagements                      = as.numeric(el$totalEngagements %||% 0),
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
## Fetch daily rows for one scope (one group OR one campaign). Never throws.
fetch_li_scope <- function(scope_label, scope_value, config, bearer_token,
                           debug = FALSE) {

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
    "  Pulling LinkedIn {scope_label}={scope_value}"
  )

  result <- tryCatch(
    run_linkedin_request(scope_param, urn, config, bearer_token, debug = debug),
    error = function(e) {
      cli::cli_alert_warning(
        "LinkedIn request failed for {scope_label}={scope_value}: {conditionMessage(e)}"
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
      "LinkedIn HTTP {result$status_code} for {scope_label}={scope_value}. Detail: {detail}"
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
      "LinkedIn returned 0 rows for {scope_label}={scope_value}."
    )
    if (!isTRUE(debug)) {
      cli::cli_alert_info(
        "(Re-run with {.code pull_linkedin(cfg, debug = TRUE)} to see the URL and raw body.)"
      )
    }
    return(tibble::tibble())
  }

  flatten_linkedin_elements(result$parsed, scope_label, scope_value)
}

## ---- Public entry point ---------------------------------------------------

#' Pull LinkedIn ad performance for a campaign
#'
#' Queries LinkedIn's Marketing API at either the campaign-group or
#' campaign-id level (or both), aggregating daily metrics into the canonical
#' weekly channel schema.
#'
#' @param config  A `campaign_config` object. Required: `linkedin.account_id`.
#'   Scope: at least one of `linkedin.campaign_groups` (vector of group IDs)
#'   or `linkedin.campaigns` (vector of campaign IDs).
#'   Optional: `linkedin.api_version` (default `"202509"`),
#'   `linkedin.token_file`, `linkedin.token_refresh_script`.
#' @param refresh Force re-fetch instead of loading from cache.
#' @param debug   If `TRUE`, prints the full request URL and the first 500
#'   chars of the response body for every scope. Useful for diagnosing
#'   empty-result cases.
#'
#' @return A weekly tibble conforming to the canonical channel schema. The
#'   `platform_conversions` column holds LinkedIn's CAPI-reported
#'   `externalWebsiteConversions` and is NOT comparable to other channels'
#'   or GA4's conversion counts.
#'
#' @export
pull_linkedin <- function(config, refresh = FALSE, debug = FALSE) {

  stopifnot(inherits(config, "campaign_config"))

  cached <- archive_load(config, "linkedin", refresh = refresh)
  if (!is.null(cached)) return(cached)

  li_cfg <- config$linkedin
  account_id      <- li_cfg$account_id
  campaign_groups <- li_cfg$campaign_groups %||% character()
  campaign_ids    <- li_cfg$campaigns       %||% character()

  if (is.null(account_id) || !nzchar(account_id)) {
    cli::cli_alert_warning(
      "LinkedIn {.field account_id} not set in config. Returning empty tibble."
    )
    out <- empty_channel_weekly("linkedin")
    archive_save(config, "linkedin", out)
    return(out)
  }

  if (length(campaign_groups) == 0 && length(campaign_ids) == 0) {
    cli::cli_alert_warning(
      "LinkedIn config has neither {.field campaign_groups} nor {.field campaigns}. Returning empty tibble."
    )
    out <- empty_channel_weekly("linkedin")
    archive_save(config, "linkedin", out)
    return(out)
  }

  ensure_linkedin_deps()
  bearer_token <- get_linkedin_access_token(config)

  cli::cli_alert_info(
    "Pulling LinkedIn: {config$start_date} \u2192 {config$end_date} (account {account_id}, {length(campaign_groups)} group{?s}, {length(campaign_ids)} campaign{?s})"
  )

  raw_groups <- purrr::map_dfr(
    campaign_groups,
    function(g) fetch_li_scope("campaign_group_id", g, config, bearer_token, debug = debug)
  )
  raw_campaigns <- purrr::map_dfr(
    campaign_ids,
    function(c) fetch_li_scope("campaign_id", c, config, bearer_token, debug = debug)
  )

  ## When the user scopes by campaigns, LinkedIn sometimes ignores the filter
  ## and returns ALL campaigns in the account. Clip to the requested IDs.
  if (nrow(raw_campaigns) > 0 && length(campaign_ids) > 0) {
    requested <- as.character(campaign_ids)
    raw_campaigns <- raw_campaigns %>%
      dplyr::filter(as.character(.data$campaign_id_lookup) %in% requested)
  }

  raw <- dplyr::bind_rows(raw_groups, raw_campaigns)

  if (nrow(raw) == 0) {
    cli::cli_alert_warning("LinkedIn returned no usable rows across all scopes.")
    out <- empty_channel_weekly("linkedin")
    archive_save(config, "linkedin", out)
    return(out)
  }

  ## Every row carries the individual campaign ID in pivotValues[1] (parsed
  ## into `campaign_id_lookup`). Deduplicate across scope types.
  raw <- raw %>%
    dplyr::mutate(campaign_id_lookup = as.character(.data$campaign_id_lookup)) %>%
    dplyr::distinct(
      .data$campaign_id_lookup, .data$event_date,
      .keep_all = TRUE
    ) %>%
    ## Drop the internal scope-tracking columns now that we've deduped.
    dplyr::select(-dplyr::any_of(c(".scope_kind", ".scope_value")))

  campaign_name_tbl <- fetch_linkedin_campaign_names(
    unique(raw$campaign_id_lookup),
    config,
    bearer_token
  )

  raw <- raw %>%
    dplyr::left_join(campaign_name_tbl, by = "campaign_id_lookup") %>%
    dplyr::rename(
      campaign_id          = "campaign_id_lookup",
      spend                = "costInLocalCurrency",
      platform_conversions = "externalWebsiteConversions"
    ) %>%
    filter_by_inventory(config, "linkedin")

  cli::cli_alert_info(
    "  LinkedIn raw rows after inventory filter: {nrow(raw)}"
  )

  if (nrow(raw) == 0) {
    out <- empty_channel_weekly("linkedin")
    archive_save(config, "linkedin", out)
    return(out)
  }

  week_index <- build_week_index(config)

  out <- raw %>%
    assign_daily_to_weeks(week_index, date_col = "event_date") %>%
    dplyr::group_by(.data$week_num, .data$weeks_to_end, .data$phase,
                    .data$week_start, .data$week_end,
                    .data$campaign_id, .data$campaign_name) %>%
    dplyr::summarise(
      spend                = sum(.data$spend,                na.rm = TRUE),
      impressions          = sum(.data$impressions,          na.rm = TRUE),
      clicks               = sum(.data$clicks,               na.rm = TRUE),
      platform_conversions = sum(.data$platform_conversions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      channel = "linkedin",
      cpc = dplyr::if_else(.data$clicks > 0,
                           .data$spend / .data$clicks,
                           NA_real_),
      cpp = dplyr::if_else(.data$platform_conversions > 0,
                           .data$spend / .data$platform_conversions,
                           NA_real_)
    ) %>%
    enforce_channel_schema("linkedin")

  archive_save(config, "linkedin", out)
  out
}
