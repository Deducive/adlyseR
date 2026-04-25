#' adlyseR: Cross-Channel Ad & Traffic Analysis Toolkit
#'
#' A reusable toolkit for pulling, harmonising and combining ad performance
#' data from Meta, Google Ads and LinkedIn, together with GA4 traffic for a
#' campaign's landing pages.
#'
#' Each campaign is described by a [`campaign_config`][load_campaign_config()]
#' object (typically loaded from a YAML file). The same code can then serve
#' multiple clients and campaigns.
#'
#' @section Typical workflow:
#' ```r
#' library(adlyseR)
#' cfg <- load_campaign_config("inst/campaigns/soi_2026.yml")
#' meta   <- pull_meta(cfg)
#' google <- pull_google_ads(cfg)
#' li     <- pull_linkedin(cfg)
#' ga4    <- pull_ga4_pagepath(cfg)
#' combined <- combine_channels(meta, google, li, ga4 = ga4, config = cfg)
#' ```
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

## Tidy-evaluation pronoun. Imported here so every internal function that
## references `.data$column` inside dplyr verbs picks it up via the package
## NAMESPACE <e2><80><94> silences "no visible binding for global variable '.data'"
## from R CMD check without needing per-file imports.
#' @importFrom rlang .data
NULL
