test_that("Filename generator produces path with generally correct formatting", {
  expect_equal(typeof(make_filename(2014)), "character")
  expect_match(make_filename(2015), "data/")
})


# test_that("Data files have expected naming and path", {
#   expect_true(file.exists(file.path(system.file("data", package="FARSaccess"), "accident_2013.csv.bz2")))
#   expect_true(file.exists(file.path(system.file("data", package="FARSaccess"), "accident_2014.csv.bz2")))
#   expect_true(file.exists(file.path(system.file("data", package="FARSaccess"), "accident_2015.csv.bz2")))
# })


