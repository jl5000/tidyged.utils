# Generated by roxytest: Do not edit by hand!

# File R/guess_age.R: @tests

test_that("Function date_diff() @ L175", {
  expect_equal(date_diff("1900", "2000"), 99, tolerance = 0.01)
  expect_equal(date_diff("1900", "2000", minimise = FALSE), 101, tolerance = 0.01)
  expect_equal(date_diff("25 JAN", "8 MAR"), -1)
  expect_equal(date_diff("800", "2020"), 1219, tolerance = 0.01)
  expect_equal(date_diff("AFT 1900", "2000"), -1)
  expect_equal(date_diff("1900", "BEF 2000"), -1)
  expect_equal(date_diff("28 JAN 2006", "14 DEC 2008"), 2.877, tolerance = 0.01)
  expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016"), 4, tolerance = 0.01)
  expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016", minimise = FALSE), 16.35, tolerance = 0.01)
  expect_equal(date_diff("ABT 1932", "CAL 2000"), 67, tolerance = 0.01)
})

