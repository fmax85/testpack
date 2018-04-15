context("Generic test")

test_that("FARS file is created appropriately", {
    expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
