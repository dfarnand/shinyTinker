test_that("infoRadioButtons creates correct HTML", {
  result <- infoRadioButtons("test", "Test Label", c("A", "B"))
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "div")
  expect_equal(result$attribs$id, "test")
})