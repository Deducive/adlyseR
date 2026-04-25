## Campaign Config ------------------------------------------------------------
## Loads a YAML file describing one campaign and returns a `campaign_config`
## object (S3 class) used by every pull_*() and combine_channels() function.

#' Load a campaign configuration
#'
#' Reads a YAML file and returns a `campaign_config` object that holds every
#' value the pull functions need (dates, account IDs, GA4 property, campaign
#' inventory path, archive directory).
#'
#' @param path Path to a YAML file describing the campaign. See
#'   `inst/campaigns/soi_2026.yml` for the expected shape.
#'
#' @return A list of class `campaign_config`.
#'
#' @examples
#' \dontrun{
#' cfg <- load_campaign_config(
#'   system.file("campaigns/soi_2026.yml", package = "adlyseR")
#' )
#' }
#'
#' @export
load_campaign_config <- function(path) {

  if (!file.exists(path)) {
    cli::cli_abort("Campaign config not found: {.path {path}}")
  }

  raw <- yaml::read_yaml(path)

  cfg <- list(
    name         = raw$name       %||% tools::file_path_sans_ext(basename(path)),
    client       = raw$client     %||% NA_character_,
    start_date   = as.Date(raw$start_date),
    end_date     = as.Date(raw$end_date),
    phases       = raw$phases,
    meta         = raw$meta         %||% list(),
    google_ads   = raw$google_ads   %||% list(),
    linkedin     = raw$linkedin     %||% list(),
    ga4          = raw$ga4          %||% list(),
    transactions = raw$transactions %||% NULL,
    inventory    = raw$inventory    %||% list(),
    archive_dir  = raw$archive_dir  %||% file.path(tempdir(), "adlyseR_archive"),
    source_path  = normalizePath(path, mustWork = TRUE)
  )

  validate_campaign_config(cfg)
  structure(cfg, class = c("campaign_config", "list"))
}

#' @noRd
validate_campaign_config <- function(cfg) {

  required <- c("name", "start_date", "end_date")
  missing_fields <- required[vapply(cfg[required], function(x)
    is.null(x) || all(is.na(x)), logical(1))]

  if (length(missing_fields) > 0) {
    cli::cli_abort(
      "Campaign config is missing required field{?s}: {.field {missing_fields}}"
    )
  }

  if (cfg$end_date < cfg$start_date) {
    cli::cli_abort(
      "Campaign {.val {cfg$name}}: {.field end_date} is before {.field start_date}."
    )
  }

  invisible(cfg)
}

#' @export
print.campaign_config <- function(x, ...) {
  cli::cli_h1("Campaign: {x$name}")
  if (!is.na(x$client)) cli::cli_text("Client: {.strong {x$client}}")
  cli::cli_text("Dates:  {x$start_date} \u2192 {x$end_date}")
  cli::cli_text("Source: {.path {x$source_path}}")

  cli::cli_h2("Channels configured")
  for (ch in c("meta", "google_ads", "linkedin", "ga4")) {
    val <- x[[ch]]
    if (length(val) == 0) {
      cli::cli_text("{.field {ch}}: {.emph (not configured)}")
    } else {
      cli::cli_text("{.field {ch}}: {length(val)} setting{?s}")
    }
  }
  invisible(x)
}

## Null-coalescing helper -----------------------------------------------------
## (magrittr / rlang provide this but we keep a tiny local copy so the utility
##  files don't need to import from rlang just for one operator.)

#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
