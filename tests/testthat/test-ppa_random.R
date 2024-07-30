if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  ES <- here::here("tests", "testthat", "testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
} else {
  ES <- test_path("testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
}
if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  ES <- here::here("tests", "testthat", "testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
} else {
  ES <- test_path("testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
}
P <- ES |> dplyr::select(c(sp1, sp2, sp3))
ES <- ES |> dplyr::select(-c(sp1, sp2, sp3))
P_sp1 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp1 > 0) |>
  dplyr::select(-c(sp2, sp3))
PA_sp1 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp1 == 0) |>
  dplyr::select(-c(sp2, sp3)) |>
  head(nrow(P_sp1))
PPA_sp1 <- PA_sp1 |> dplyr::bind_rows(P_sp1)
P_sp2 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp2 > 0) |>
  dplyr::select(-c(sp1, sp3))
PA_sp2 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp2 == 0) |>
  dplyr::select(-c(sp1, sp3)) |>
  head(nrow(P_sp2))
PPA_sp2 <- PA_sp2 |> dplyr::bind_rows(P_sp2)
P_sp3 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp3 > 0) |>
  dplyr::select(-c(sp1, sp2))
PA_sp3 <- ES |>
  dplyr::bind_cols(P) |>
  dplyr::filter(sp3 == 0) |>
  dplyr::select(-c(sp1, sp2)) |>
  head(nrow(P_sp3))
PPA_sp3 <- PA_sp3 |> dplyr::bind_rows(P_sp3)



test_that("ES is a dataframe with 0 rows", {
  expect_error(
    PPA_sp1 |>
      dplyr::filter(sp1==1) |>
      ppa_random(P_sp1, ES_idx = "cell_id"),
    "Assertion on 'ES' failed: Must have at least 1 rows, but has 0 rows."
  )
})


