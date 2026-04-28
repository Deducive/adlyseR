# adlyseR

Cross-channel ad-and-traffic analysis toolkit. Pulls Meta, Google Ads,
LinkedIn, and GA4 data for a given campaign and combines them into a
tidy weekly dataset.

## Status

**Pre-alpha, scaffolded from working patterns in
`nacs-attribution/ns-media-planning`.** First user: the NACS State of
the Industry 2026 post-event wrap-up.

Currently this package lives under `nacs-attribution/adlyseR/` for
convenience. It will move to `~/Code/GitHub/adlyseR/` once the API
settles down.

## Design

- One campaign = one YAML config file.
- Each `pull_*()` function takes a `campaign_config` and returns a tibble
  conforming to `channel_weekly_schema`.
- `combine_channels()` stacks them into `spend_long`, sums to
  `weekly_total`, and joins GA4 for a `weekly_combined` view.
- Every pull is RDS-cached under `config$archive_dir`. Force a refresh
  with `refresh = TRUE` or `Sys.setenv(ADLYSER_REFRESH = "TRUE")`.

## Quick start

```r
# devtools::load_all("adlyseR")  # while iterating

library(adlyseR)

cfg <- load_campaign_config("adlyseR/inst/campaigns/soi_2026.yml")
print(cfg)

# Meta: source your existing fb_insights_convs() first
source("functions/fb_conversions_function.R")
meta <- pull_meta(cfg)

# Google Ads: authenticate once with gads_auth() in an interactive session
google <- pull_google_ads(cfg)

# LinkedIn: auth via the token file / refresh script referenced in YAML
li <- pull_linkedin(cfg)

# GA4 /SOI/ traffic
ga4_soi <- pull_ga4_pagepath(cfg)

# Combine
combined <- combine_channels(meta, google, li, ga4 = ga4_soi, config = cfg)

combined$weekly_combined
```

## Configuration values and tokens

Campaign settings are read from YAML via `load_campaign_config()`. Values can
be written directly in the YAML, or read from environment variables with the
`env:VAR_NAME` form. Prefer environment variables for tokens and local
credential paths so secrets do not get committed.

```yaml
name: soi_2026
start_date: "2026-01-01"
end_date: "2026-04-15"

meta:
  account_id: "env:META_ACCOUNT_ID"
  access_token: "env:META_ACCESS_TOKEN"

google_ads:
  customer_id: "env:GOOGLE_ADS_CUSTOMER_ID"
  login_customer_id: "env:GOOGLE_ADS_LOGIN_CUSTOMER_ID"

linkedin:
  account_id: "env:LINKEDIN_ACCOUNT_ID"
  campaign_groups:
    - "688096446"
  token_file: "env:LINKEDIN_TOKEN_FILE"
  token_refresh_script: "env:LINKEDIN_REFRESH_SCRIPT"

ga4:
  property_id: "env:GA4_PROPERTY_ID"
  service_account_json: "env:GA4_SERVICE_ACCOUNT_JSON"
  hostnames:
    - "www.example.com"
  page_path_prefix: "/SOI/"
```

Set those variables before loading the config:

```r
Sys.setenv(
  META_ACCOUNT_ID = "act_123",
  META_ACCESS_TOKEN = "EAAB...",
  GOOGLE_ADS_CUSTOMER_ID = "1234567890",
  GA4_PROPERTY_ID = "123456789"
)

cfg <- load_campaign_config("campaigns/soi_2026.yml")
```

Channel-specific auth still follows each API package's normal setup:

- **Meta:** `meta.account_id` and `meta.access_token` can come from YAML or
  `env:` values. If omitted, `pull_meta()` falls back to global
  `fb_account` and `fb_access_token` objects.
- **Google Ads:** set `google_ads.customer_id` and optionally
  `google_ads.login_customer_id`; authenticate interactively with
  `rgoogleads::gads_auth()` once in the R session.
- **LinkedIn:** set `linkedin.account_id` plus either campaign groups or
  campaigns. Provide `linkedin.token_file` for a CSV token cache, or
  `linkedin.token_refresh_script` for a script that defines `a.token`.
- **GA4:** set `ga4.property_id`. If `ga4.service_account_json` points to a
  JSON key file, `pull_ga4_*()` authenticates with it; otherwise it uses the
  existing `googleAnalyticsR::ga_auth()` state.

## Port status

| Script in ns-media-planning | adlyseR function        | Status        |
|-----------------------------|------------------------|---------------|
| `ns_setup.R`                | `load_campaign_config`, `build_week_index`, `utils_*` | Ported |
| `ns_pull_meta.R`            | `pull_meta()`          | Ported (needs live test) |
| `ns_pull_google_ads.R`      | `pull_google_ads()`    | Ported (needs live test) |
| `ns_pull_ga4.R`             | `pull_ga4_sessions()`, `pull_ga4_pagepath()` | Ported; page-path is new |
| `ns_pull_linkedin.R`        | `pull_linkedin()`      | Ported (needs live test) |
| `ns_pull_trends.R`          | <e2><80><94>                      | Not yet ported |
| `ns_pull_search_console.R`  | <e2><80><94>                      | Not yet ported |
| `ns_combine.R`              | `combine_channels()`   | Simplified port |
| Campaign inventory CSV      | `utils_inventory.R`    | Ported |

## Follow-ups

1. Add unit tests (`tests/testthat/`).
2. Port the Careismatic reporting scaffold (`_template.qmd`,
   `findings.qmd`, `render.R`) into `inst/templates/`.
3. Move the package from `nacs-attribution/adlyseR/` to
   `~/Code/GitHub/adlyseR/` and add `usethis::use_mit_license()`.
4. Add Google Search Console and Trends pulls as opt-in.
5. Live-test each pull against the SOI 2026 data and harden error
   handling based on what comes back.
