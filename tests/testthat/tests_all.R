test_that("Filename generator produces path with generally correct formatting", {
  expect_equal(typeof(make_filename(2014)), "character")
  expect_match(make_filename(2015), "data/")
})




