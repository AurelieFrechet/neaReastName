test_that("Full match", {
  # Full match
  expect_equal(pertinence(c("A", "A")),
               1)
  expect_equal(pertinence(c("A", "A", "A")),
               1)
  expect_equal(pertinence(c("A", "A", "A", "A")),
               1)
  expect_equal(pertinence(c("A", "A", "A", "A", "A")),
               1)
})

test_that("No match", {
  # Full match
  expect_equal(pertinence(c(NA, NA)),
               0)

  expect_equal(pertinence(c(NA, NA, NA)),
               0)

  expect_equal(pertinence(c(NA, NA, NA, NA)),
               0)

  expect_equal(pertinence(c(NA, NA, NA, NA, NA)),
               0)
})

test_that("1 center missing name", {

  # 5 names
  expect_equal(pertinence(c("JEAN", NA, "DE", "LA", "FESANDIERE")),
               1 - 0.1)
  expect_equal(pertinence(c("JEAN", "BERNAR", NA, "LA", "FESANDIERE")),
               1 - 0.1)
  expect_equal(pertinence(c("JEAN", "BERNAR", "DE", NA, "FESANDIERE")),
               1 - 0.1)
  # 4 names
  expect_equal(pertinence(c("AURELIE", NA, "JULIE", "FRECHET")),
               1 - 0.1)
  expect_equal(pertinence(c("AURELIE", "LAURA", NA, "FRECHET")),
               1 - 0.1)
  # 3 names
  expect_equal(pertinence(c("SULIAC", NA, "GUILLOU")),
               1 - 0.1)

})

test_that("2 center missing names", {
  # 5 names
  expect_equal(pertinence(c("JEAN", NA, NA, "LA", "FESANDIERE")),
               1 - 2*0.1)
  expect_equal(pertinence(c("JEAN", "BERNAR", NA, NA, "FESANDIERE")),
               1 - 2*0.1)
  expect_equal(pertinence(c("JEAN", NA, "DE", NA, "FESANDIERE")),
               1 - 2*0.1)
  # 4 names
  expect_equal(pertinence(c("AURELIE", NA, NA, "FRECHET")),
               1 - 2*0.1)
})

test_that("3 center missing names", {
  # 5 names
  expect_equal(pertinence(c("JEAN", NA, NA,  NA, "FESANDIERE")),
               1 - 3*0.1)
})


