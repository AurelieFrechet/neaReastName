list_of_names = list(
  c("ASTON", "MARTINE"),
  c("RENAULT", "ZOE"),
  c("BATMAN"),
  c("BAT", "MAN"),
  c("CLARK", "KENT"),
  c("KLARK", "KENT"),
  c("SUPERMAN"),
  c("SUPER", "MAN"),
  c("MARTIN", "ASTONN"),
  c("ZOE", "RENAUD"),
  c("ALICE", "BOB"),
  c("ALICIA", "MARIE", "MARLENE"),
  c("BART", "MANN"),
  c("MARIE", "LILY", "MARLEEN"),
  c("LILI", "MARLENE"),
  c("ABOT", "EUGENE"),
  c("ABOT", "EUGENE", "MICHEL", "JOSEPH"),
  c("ABOUT", "SUZANNE"),
  c("ABEL", "PUJOL", "FILS", "ALEXANDRE"),
  c("ABEL", "PUJOL", "ALEXANDRE", "DENIS"),
  c("ABEL", "PUJOL", "ADRIENNE", "MARIE", "LOUISE", "GRANDPIERRE", "DEVERZY"),
  c("ABEL", "DUFRESNE"),
  c("L", "MARLENE")
)


test_that("LILI MARLENE", {
  expect_equal(match_names(
    name = c("LILI", "MARLENE"),
    list_of_names = list_of_names,
    last_name = TRUE
  ),
  list(c("MARIE", "LILY", "MARLEEN"),
       c("LILI", "MARLENE")))

})


test_that("BATMAN", {
  expect_equal(match_names(name = c("BATMAN"),
                           list_of_names = list_of_names),
               list(c("BATMAN"),
                    c("BAT", "MAN")))
})

test_that("ABOT ABOUT", {
  expect_equal(match_names(name = c("ABOT", "EUGENE", "MICHEL", "JOSEPH"),
                           list_of_names = list_of_names,
                           last_name = FALSE),
               list(c("ABOT", "EUGENE"),
                    c("ABOT", "EUGENE", "MICHEL", "JOSEPH")))
  expect_equal(match_names(name = c("ABOUT", "SUZANNE"),
                           list_of_names = list_of_names,
                           last_name = FALSE),
               list(c("ABOUT", "SUZANNE")))
})

