## Archive Cache -------------------------------------------------------------
## Every pull_*() and combine_channels() caches its result as an RDS file
## under config$archive_dir. Set the `ADLYSER_REFRESH` env var to "TRUE"
## (or pass `refresh = TRUE`) to force a fresh pull.

#' @noRd
archive_refresh_flag <- function() {
  identical(toupper(Sys.getenv("ADLYSER_REFRESH", unset = "FALSE")), "TRUE")
}

#' @noRd
archive_path <- function(config, stem) {
  file.path(config$archive_dir, paste0(config$name, "__", stem, ".rds"))
}

#' @noRd
archive_load <- function(config, stem, refresh = FALSE) {

  path <- archive_path(config, stem)
  if (refresh || archive_refresh_flag() || !file.exists(path)) {
    return(NULL)
  }

  cli::cli_alert_info("Loading cached {.val {stem}} from {.path {path}}")
  tryCatch(
    readRDS(path),
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to load archive {.path {path}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

#' @noRd
archive_save <- function(config, stem, object) {

  dir.create(config$archive_dir, recursive = TRUE, showWarnings = FALSE)
  path <- archive_path(config, stem)

  tryCatch(
    {
      saveRDS(object, path)
      cli::cli_alert_success("Saved {.val {stem}} to {.path {path}}")
      invisible(path)
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to save archive for {.val {stem}}: {conditionMessage(e)}"
      )
      invisible(NULL)
    }
  )
}
