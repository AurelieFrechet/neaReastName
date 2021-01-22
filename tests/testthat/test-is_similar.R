test_that("is_similar", {
  expect_true(is_similar("CHAT", "CHTA",
                          threshold = 1))
  expect_false(is_similar("CHAT", "CHTA",
                         threshold = 0))
})
