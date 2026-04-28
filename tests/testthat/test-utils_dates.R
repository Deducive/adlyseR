test_that("build_week_index uses zero for final week", {
  cfg <- structure(
    list(
      start_date = as.Date("2026-01-01"),
      end_date = as.Date("2026-01-14"),
      phases = list()
    ),
    class = c("campaign_config", "list")
  )

  weeks <- build_week_index(cfg)

  expect_identical(weeks$weeks_to_end, c(1L, 0L))
})

test_that("build_week_index handles a one-week campaign", {
  cfg <- structure(
    list(
      start_date = as.Date("2026-01-01"),
      end_date = as.Date("2026-01-07"),
      phases = list()
    ),
    class = c("campaign_config", "list")
  )

  weeks <- build_week_index(cfg)

  expect_identical(weeks$weeks_to_end, 0L)
})
