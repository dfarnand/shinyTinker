test_that("triStateCheckboxInput creates correct HTML", {
  result <- triStateCheckboxInput("test", "Test Label", 0)
  expect_s3_class(result, "shiny.tag")
})

test_that("triStateCheckboxInput renders a native checkbox input", {
  html <- as.character(
    htmltools::renderTags(triStateCheckboxInput("test", "Test Label", 0))$html
  )
  expect_match(html, "form-group shiny-input-container")
  expect_match(html, "<label")
  expect_match(html, 'type="checkbox"')
  expect_match(html, 'id="test"')
  expect_match(html, 'class="tri-state-checkbox"')
  expect_match(html, 'data-state="0"')
  expect_match(html, "Test Label")
  expect_no_match(html, "checked=")
})

test_that("triStateCheckboxInput emits checked only for include", {
  html_include <- as.character(
    htmltools::renderTags(triStateCheckboxInput("test", "Label", 1))$html
  )
  expect_match(html_include, 'data-state="1"')
  expect_match(html_include, "checked")

  html_exclude <- as.character(
    htmltools::renderTags(triStateCheckboxInput("test", "Label", -1))$html
  )
  expect_match(html_exclude, 'data-state="-1"')
  expect_no_match(html_exclude, "checked=")
})

test_that("triStateCheckboxInput rejects invalid values", {
  expect_error(triStateCheckboxInput("test", "Label", 2))
})

test_that("getTriStateLabel returns correct labels", {
  expect_equal(getTriStateLabel(-1), "Exclude")
  expect_equal(getTriStateLabel(0), "Neutral")
  expect_equal(getTriStateLabel(1), "Include")
  expect_equal(getTriStateLabel(2), "Unknown")
})
