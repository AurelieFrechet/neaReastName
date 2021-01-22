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
  c("LILI", "MARLENE")
)


test_that("LILI MARLENE", {
  match_names(name = c("LILI", "MARLENE"),
              list_of_names = list_of_names)
})
