test_that("concat_names", {
  expect_equal(concat_namedist(name1 = c("BOB", "DYLAN"),
                  name2 = "BOBDYLAN"),
               0)
})
