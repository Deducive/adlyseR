test_that("flatten_linkedin_creative_elements extracts campaign and creative pivots", {
  parsed <- list(
    elements = list(
      list(
        pivotValues = c(
          "urn:li:sponsoredCampaign:123",
          "urn:li:sponsoredCreative:456"
        ),
        clicks = 10,
        costInLocalCurrency = "25.50",
        impressions = 1000,
        externalWebsiteConversions = 4,
        externalWebsitePostClickConversions = 3,
        externalWebsitePostViewConversions = 1,
        totalEngagements = 20,
        dateRange = list(
          start = list(year = 2026, month = 5, day = 12)
        )
      )
    )
  )

  out <- flatten_linkedin_creative_elements(
    parsed,
    scope_label = "campaign_group_id",
    scope_value = "789"
  )

  expect_equal(nrow(out), 1)
  expect_identical(out$campaign_id_lookup, "123")
  expect_identical(out$creative_id_lookup, "456")
  expect_equal(out$spend, 25.5)
  expect_equal(out$externalWebsitePostClickConversions, 3)
  expect_equal(out$externalWebsitePostViewConversions, 1)
  expect_equal(out$event_date, as.Date("2026-05-12"))
})
