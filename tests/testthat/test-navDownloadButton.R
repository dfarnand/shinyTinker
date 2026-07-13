test_that("navDownloadButton creates correct HTML", {
  result <- navDownloadButton("test")
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "li")
  expect_equal(result$attribs$class, "nav-item dropdown")

  link <- result$children[[1]]
  expect_equal(link$name, "a")
  expect_equal(link$attribs$id, "test")
  expect_match(link$attribs$class, "shiny-download-link")
})

test_that("navDownloadButton tooltip attributes are optional", {
  with_tooltip <- navDownloadButton("test", tooltip = "Get the data")
  link <- with_tooltip$children[[1]]
  expect_equal(link$attribs$`data-toggle`, "tooltip")
  expect_equal(link$attribs$title, "Get the data")

  no_tooltip <- navDownloadButton("test", tooltip = NULL)
  link <- no_tooltip$children[[1]]
  expect_null(link$attribs$`data-toggle`)
  expect_null(link$attribs$title)
})
