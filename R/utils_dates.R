## Week Index ----------------------------------------------------------------
## Ported from ns-media-planning/ns_setup.R::build_week_index(). Adapted to
## work from a campaign_config instead of a hard-coded year lookup.

#' Build a weekly index for a campaign
#'
#' Returns a tibble with one row per week between a campaign's start and end
#' date. Columns include `week_num`, `weeks_to_end`, `week_start`, `week_end`,
#' and an optional `phase` label if `config$phases` is provided.
#'
#' A "week" is a 7-day interval anchored on `config$start_date`.
#'
#' @param config A `campaign_config` object from [load_campaign_config()].
#'
#' @return A tibble. Columns:
#'   \describe{
#'     \item{week_num}{Sequential week number from campaign start (1 = first week).}
#'     \item{weeks_to_end}{Weeks remaining until end_date (0 = final week).}
#'     \item{week_start, week_end}{Calendar dates (week_end = week_start + 6 days).}
#'     \item{phase}{Phase label if defined in config, otherwise NA.}
#'   }
#'
#' @export
build_week_index <- function(config) {

  stopifnot(inherits(config, "campaign_config"))

  weeks <- seq.Date(config$start_date, config$end_date, by = "week") %>%
    tibble::enframe(value = "week_start", name = "week_num") %>%
    dplyr::mutate(
      week_end     = .data$week_start + 6L,
      weeks_to_end = pmax(
        as.integer(ceiling(as.numeric(config$end_date - .data$week_start) / 7)) - 1L,
        0L
      )
    )

  weeks <- assign_phases(weeks, config)

  weeks %>%
    dplyr::select(
      dplyr::all_of(c("week_num", "weeks_to_end", "week_start", "week_end", "phase"))
    )
}

#' @noRd
## Assign a phase label to each week based on config$phases.
##
## config$phases is expected as a list of named entries:
##   phases:
##     early_bird: { start_date: "2025-11-01", end_date: "2025-12-15" }
##     launch_push: { start_date: "2025-12-16", end_date: "2026-01-10" }
##     final_push:  { start_date: "2026-03-20", end_date: "2026-04-15" }
##
## Week's phase = the first phase whose date range contains week_start.
## Overlapping phases should therefore be ordered deliberately in YAML.
assign_phases <- function(week_tbl, config) {

  if (length(config$phases) == 0) {
    return(dplyr::mutate(week_tbl, phase = NA_character_))
  }

  phase_names <- names(config$phases)
  phase_starts <- as.Date(vapply(config$phases,
                                 function(p) p$start_date %||% NA,
                                 character(1)))
  phase_ends   <- as.Date(vapply(config$phases,
                                 function(p) p$end_date %||% NA,
                                 character(1)))

  phase_for <- function(ws) {
    hits <- which(ws >= phase_starts & ws <= phase_ends)
    if (length(hits) == 0) return(NA_character_)
    phase_names[hits[1]]
  }

  dplyr::mutate(
    week_tbl,
    phase = factor(vapply(.data$week_start, phase_for, character(1)),
                   levels = phase_names)
  )
}

## Fuzzy-assign daily rows to week buckets ------------------------------------
## Used by every pull_* function that returns daily data.

#' @noRd
assign_daily_to_weeks <- function(daily_df, week_index, date_col = "event_date") {

  stopifnot(is.data.frame(daily_df), is.data.frame(week_index))

  if (nrow(daily_df) == 0) {
    return(daily_df)
  }

  by_spec <- c("week_start", "week_end")
  names(by_spec) <- c(date_col, date_col)

  daily_df %>%
    fuzzyjoin::fuzzy_left_join(
      week_index,
      by        = by_spec,
      match_fun = list(`>=`, `<=`)
    ) %>%
    dplyr::filter(!is.na(.data$week_start))
}
