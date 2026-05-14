## Campaign name matching -----------------------------------------------------

#' Match campaign names against required token groups
#'
#' Checks that each required group has at least one token present in a campaign
#' name. Matching is case-insensitive and token order does not matter.
#'
#' @param campaign_name Character vector of campaign names.
#' @param required_groups A named or unnamed list of character vectors. Tokens
#'   are ORed within each group and ANDed across groups.
#'
#' @return Logical vector the same length as `campaign_name`.
#'
#' @examples
#' match_campaign_name(
#'   c("2026 NACS Show", "NACS Membership", "Show - NACS - 26"),
#'   list(year = c("26", "2026"), "NACS", "Show")
#' )
#'
#' @export
match_campaign_name <- function(campaign_name,
                                required_groups = list(
                                  year = c("26", "2026"),
                                  "NACS",
                                  "Show"
                                )) {
  campaign_name <- as.character(campaign_name)
  haystack <- stringr::str_to_lower(campaign_name)

  group_hits <- lapply(required_groups, function(tokens) {
    tokens <- stringr::str_to_lower(as.character(tokens))
    tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
    if (length(tokens) == 0) return(rep(TRUE, length(haystack)))

    Reduce(
      `|`,
      lapply(tokens, function(token) {
        stringr::str_detect(haystack, stringr::fixed(token))
      }),
      init = rep(FALSE, length(haystack))
    )
  })

  out <- Reduce(`&`, group_hits, init = rep(TRUE, length(haystack)))
  out[is.na(out)] <- FALSE
  out
}

#' @noRd
filter_campaign_names <- function(df, required_groups = NULL,
                                  campaign_name_col = "campaign_name") {
  if (is.null(required_groups)) return(df)
  if (!campaign_name_col %in% names(df)) return(df)

  df %>%
    dplyr::filter(
      match_campaign_name(.data[[campaign_name_col]], required_groups)
    )
}
