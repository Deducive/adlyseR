test_that("flatten_linkedin_conversion_elements extracts conversion pivots", {
  parsed <- list(
    elements = list(
      list(
        pivotValues = c(
          "urn:li:sponsoredCampaign:123",
          "urn:lla:llaPartnerConversion:456"
        ),
        externalWebsiteConversions = 7,
        externalWebsitePostClickConversions = 5,
        externalWebsitePostViewConversions = 2,
        dateRange = list(
          start = list(year = 2026, month = 3, day = 9)
        )
      )
    )
  )

  out <- flatten_linkedin_conversion_elements(
    parsed,
    scope_label = "campaign_group_id",
    scope_value = "789"
  )

  expect_equal(nrow(out), 1)
  expect_identical(out$campaign_id_lookup, "123")
  expect_identical(out$conversion_id_lookup, "456")
  expect_identical(out$conversion_urn, "urn:lla:llaPartnerConversion:456")
  expect_equal(out$externalWebsiteConversions, 7)
  expect_equal(out$externalWebsitePostClickConversions, 5)
  expect_equal(out$externalWebsitePostViewConversions, 2)
  expect_equal(out$event_date, as.Date("2026-03-09"))
})

test_that("flatten_linkedin_conversion_elements handles empty responses", {
  out <- flatten_linkedin_conversion_elements(
    list(elements = list()),
    scope_label = "campaign_id",
    scope_value = "123"
  )

  expect_equal(nrow(out), 0)
  expect_named(out, c(".scope_kind", ".scope_value"))
})
