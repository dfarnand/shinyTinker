test_that("infoRadioButtons creates correct HTML", {
  result <- infoRadioButtons("test", "Test Label", c("A", "B"))
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "div")
  expect_equal(result$attribs$id, "test")
})

test_that("label_width sets a fixed label width and disables shrinking", {
  html <- as.character(
    infoRadioButtons("test", "Test Label", c("A", "B"), label_width = "190px")
  )
  expect_match(html, "width:190px", fixed = TRUE)
  expect_match(html, "flex-shrink: 0;", fixed = TRUE)
})

test_that("labels auto-size by default", {
  html <- as.character(infoRadioButtons("test", "Test Label", c("A", "B")))
  expect_no_match(html, "width:190px", fixed = TRUE)
  expect_no_match(html, "flex-shrink: 0;", fixed = TRUE)
})

test_that("label_width accepts numeric values as pixels", {
  html <- as.character(
    infoRadioButtons("test", "Test Label", c("A", "B"), label_width = 190)
  )
  expect_match(html, "width:190px", fixed = TRUE)
})