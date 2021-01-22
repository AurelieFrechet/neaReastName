test_that("namedist", {
  expect_equal(namedist(toupper("Wachowsky"),
           toupper("Wachowskis")), 1)
})
