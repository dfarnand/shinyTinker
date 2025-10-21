test_that("triStateCheckboxInput creates correct HTML", {
  result <- triStateCheckboxInput("test", "Test Label", 0)
  expect_s3_class(result, "shiny.tag")
})

test_that("getTriStateLabel returns correct labels", {
  expect_equal(getTriStateLabel(-1), "Exclude")
  expect_equal(getTriStateLabel(0), "Neutral")
  expect_equal(getTriStateLabel(1), "Include")
  expect_equal(getTriStateLabel(2), "Unknown")
})
