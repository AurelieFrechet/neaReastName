
# Common initials ---------------------------------------------------------

test_that("Common Initials, ordered", {

  expect_equal(common_names(
    name1 = c("AURELIE", "FRECHET"),
    name2 = c("FRECHET", "AURELIE"),
    initials_only = TRUE
  ),
  c("A", NA_character_))


  expect_equal(common_names(
    name1 = c("AURELIE", "FRECHET"),
    name2 = c("ARSENE", "LUPIN"),
    initials_only = TRUE
  ),
  c("A", NA_character_))

  expect_equal(common_names(
    name1 = c("AURELIE", "FRECHET"),
    name2 = c("AURELIE", "FRECHET"),
    initials_only = TRUE
  ),
  c("A", "F"))

  expect_equal(common_names(
    name1 = c("AURELIE", "FRECHET"),
    name2 = c("AURELIE", "LAURA", "FRECHET"),
    initials_only = TRUE
  ),
  c("A", NA_character_,  "F"))

  expect_equal(common_names(
    name1 = c("AURELIE", "LAURA", "FRECHET"),
    name2 = c("AURELIE", "FRECHET"),
    initials_only = TRUE
  ),
  c("A", NA_character_,  "F"))

  expect_equal(common_names(
    name1 = c("FRECHET", "AURELIE"),
    name2 = c("AURELIE", "LAURA", "FRECHET"),
    initials_only = TRUE
  ),
  c("A", NA_character_,  NA_character_))

})


# Common names - threshold 1 -------------------------------------------------


test_that("Common names, ordered, threshold 1, r_letters default", {
  # Same number of names
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SULIAK", "LE", "GUILLOU"),
    threshold = 1,
    order = T
  ), c("SULIAC", "LE", "GUILLOU"))

  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SUYAC", "LE", "GUILLOU"),
    threshold = 1,
    order = T
  ), c("SULIAC", "LE", "GUILLOU"))

  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SULYAK", "LE", "GUYLLOU"),
    threshold = 1,
    order = T
  ), c("SULIAC", "LE", "GUILLOU"))

  expect_equal(common_names(
    name1 = c("SULLIAC", "LEGUILLOU"),
    name2 = c("SULIAC", "GUILLOU"),
    threshold = 1,
    order = T
  ), c("SULLIAC", NA_character_))

  # common_nameserent lengths
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SULLIAC", "LEGUILLOU"),
    threshold = 1,
    order = T
  ),
  c("SULIAC", NA_character_, NA_character_))

  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("DONA_character_LD", "KNUTH"),
    threshold = 1,
    order = T
  ),
  c(NA_character_, NA_character_, NA_character_))

})

# Common names - margin 2 -------------------------------------------------

test_that("Common names, ordered, threshold 2, r_letters default", {

  expect_equal(common_names(
    name1 = c("SULLIAC", "LEGUILLOU"),
    name2 = c("SULIAC", "GUILLOU"),
    threshold = 2,
    order = T
  ), c("SULLIAC", "LEGUILLOU"))

  # common_nameserent lengths
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SULLIAC", "LEGUILLOU"),
    threshold = 2,
    order = T
  ),
  c("SULIAC", NA_character_, "GUILLOU"))

  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("DONA_character_LD", "KNUTH"),
    threshold = 2,
    order = T
  ),
  c(NA_character_, NA_character_, NA_character_))

})

# Common names - theshold 0 -------------------------------------------------

test_that("Common names, ordered, threshold 0, r_letters default", {
  # Same number of names

  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("SUYAK", "LE", "GUYLLOU"),
    threshold = 0,
    order = T
  ), c(NA_character_, "LE", "GUILLOU"))



})


# Common names - non ordered ------------------------------------------------

test_that("Common names, non ordered, threshold 1, r_letters default", {
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("LE", "GUYLLOU", "SULIAK"),
    threshold = 1,
    order = F
  ), c("SULIAC", "LE", "GUILLOU"))
})

test_that("Common names, non ordered, threshold 1, r_letters default", {
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("DONALD", "KNUTH"),
    threshold = 1,
    order = F
  ), c(NA_character_, NA_character_, NA_character_))
})

test_that("Common names, non ordered, threshold 1, r_letters default", {
  expect_equal(common_names(
    name1 = c("SULIAC", "LE", "GUILLOU"),
    name2 = c("LEO", "DI", "CAPRIO"),
    threshold = 1,
    order = F
  ), c(NA_character_, "LE", NA_character_))
})

