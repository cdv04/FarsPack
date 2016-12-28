context("string character")

test_that ("Make_filename function returns right file names",{

  expect_match(make_filename(2015),"accident_2015.csv.bz2")
})
