## Campaign Inventory Filter -------------------------------------------------
## Each client typically runs many campaigns on each ad network; only a
## subset belong to the campaign we're analysing. The inventory CSV is a
## human-curated allowlist with columns:
##   network, campaign_id, campaign_name, keep
##
## Ported from ns-media-planning/ns_setup.R::ns_filter_campaign_inventory().

#' @noRd
normalize_campaign_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    dplyr::na_if("")
}

#' @noRd
## Load the inventory CSV for one network from config$inventory$path.
## Returns a tibble of (campaign_id, campaign_name) kept for this campaign,
## or NULL if no inventory file is configured.
load_inventory <- function(config, network_name) {

  path <- config$inventory$path
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }

  inv <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      network       = stringr::str_to_lower(.data$network),
      campaign_id   = normalize_campaign_id(.data$campaign_id),
      campaign_name = stringr::str_squish(.data$campaign_name),
      keep          = tidyr::replace_na(.data$keep, FALSE)
    )

  inv %>%
    dplyr::filter(
      .data$network == stringr::str_to_lower(network_name),
      .data$keep
    ) %>%
    dplyr::distinct(.data$campaign_id, .data$campaign_name)
}

#' @noRd
## Filter a daily channel tibble down to just the campaigns on the allowlist.
## If no inventory is configured, returns the input unchanged.
filter_by_inventory <- function(daily_df, config, network_name,
                                campaign_id_col = "campaign_id",
                                campaign_name_col = "campaign_name") {

  inventory_keep <- load_inventory(config, network_name)
  if (is.null(inventory_keep)) {
    return(daily_df)
  }

  if (nrow(inventory_keep) == 0) {
    cli::cli_alert_warning(
      "No campaigns marked keep=TRUE for {.field {network_name}} in inventory. Returning zero rows."
    )
    return(dplyr::slice(daily_df, 0))
  }

  keep_ids <- inventory_keep %>%
    dplyr::filter(!is.na(.data$campaign_id)) %>%
    dplyr::pull(.data$campaign_id)

  keep_names <- inventory_keep %>%
    dplyr::filter(!is.na(.data$campaign_name), nzchar(.data$campaign_name)) %>%
    dplyr::pull(.data$campaign_name)

  daily_df %>%
    dplyr::mutate(
      .id   = normalize_campaign_id(.data[[campaign_id_col]]),
      .name = stringr::str_squish(as.character(.data[[campaign_name_col]]))
    ) %>%
    dplyr::filter(
      .data$.id %in% keep_ids |
        (!is.na(.data$.name) & .data$.name %in% keep_names)
    ) %>%
    dplyr::select(-".id", -".name")
}
