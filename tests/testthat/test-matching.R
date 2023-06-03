test_that("Call API for known match", {
  expect_equal(call_name_match_api("Rhododendron ponticum L.")$data$taxonNameMatch$match$id, "wfo-0000400178" )
})

test_that("Call API for known non-match", {
  expect_equal(call_name_match_api("Banana cake Hyam")$data$taxonNameMatch$match$id, NULL)
})

test_that("Call API for known non-match but genus does match", {
  expect_equal(call_name_match_api("Rhododendron cake Hyam", fallback_to_genus = TRUE)$data$taxonNameMatch$match$id, "wfo-4000033027")
})

test_that("Call correct match method returned", {
  expect_equal(wfo_match_name("Rhododendron ponticum L.")$method, "AUTO")
})

