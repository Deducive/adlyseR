test_that("load_campaign_config resolves env-prefixed values", {
  withr::local_envvar(ADLYSER_TEST_TOKEN = "secret-token")

  cfg_file <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "name: test",
      "start_date: '2026-01-01'",
      "end_date: '2026-01-07'",
      "meta:",
      "  access_token: 'env:ADLYSER_TEST_TOKEN'"
    ),
    cfg_file
  )

  cfg <- load_campaign_config(cfg_file)

  expect_identical(cfg$meta$access_token, "secret-token")
})

test_that("load_campaign_config errors on missing env-prefixed values", {
  withr::local_envvar(ADLYSER_MISSING_TOKEN = NA)

  cfg_file <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "name: test",
      "start_date: '2026-01-01'",
      "end_date: '2026-01-07'",
      "meta:",
      "  access_token: 'env:ADLYSER_MISSING_TOKEN'"
    ),
    cfg_file
  )

  expect_error(
    load_campaign_config(cfg_file),
    "ADLYSER_MISSING_TOKEN"
  )
})
