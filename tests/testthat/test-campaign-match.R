test_that("match_campaign_name ORs within groups and ANDs across groups", {
  names <- c(
    "2026 NACS Show",
    "Show - NACS - 26",
    "2026 Expo",
    "NACS Membership",
    "Show Daily"
  )

  expect_identical(
    match_campaign_name(names, list(year = c("26", "2026"), "NACS", "Show")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("match_campaign_name is case insensitive", {
  expect_true(match_campaign_name("paid social - nacs show - 26"))
})
