if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  ES <- here::here("tests", "testthat", "testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
} else {
  ES <- test_path("testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
}
P <- ES |> dplyr::select(c(sp1, sp2, sp3))
ES <- ES |> dplyr::select(-c(sp1, sp2, sp3))

test_that("ppa_random.data.frame - ES_idx not in ES.", {
  expect_error(
    ES |>
      ppa_random(P, ES_idx = "foo"),
    "x Assertion on ES_idx failed.
i Must be element of set"
  )
})




test_that("ppa_random.data.frame - P is a vector that does not match any column in ES.", {
  expect_error(
    ES |>
      dplyr::bind_cols(P) |>
      ppa_random(c("foo")),
    "x Assertion on names"
  )
})

test_that("ppa_random.data.frame - P is a vector that match the ES_idx column in ES.", {
  expect_error(
    ES |>
      dplyr::bind_cols(P) |>
      ppa_random(c("cell_id"),
                   ES_idx = "cell_id"),
    "P does not contains species names."
  )
})

test_that("ppa_random.data.frame - cell_id of P does not join any row in ES", {
  P_tmp <- P |> dplyr::bind_cols(ES |> dplyr::select(cell_id))
  P_tmp$cell_id <- 1
  expect_error(
    ES |>
      ppa_random(
        P_tmp,
        ES_idx = "cell_id"
      ),
    "40222 rows of P can't be joined to ES by cell_id"
  )
})

test_that("ppa_random.data.frame - P is a vector that match the ES_idx column in ES.", {
  expect_error(
    ES |>
      ppa_random(P |> dplyr::filter(sp1 > 0)),
    "It was not possible to bind columns from ES and P"
  )
})

test_that("ppa_random.data.frame - ES is a dataframe with 0 rows", {
  expect_error(
    ES |>
      dplyr::filter(cell_id == -1) |>
      ppa_random(P),
    "x Assertion on ES failed.
i Must have at least 40222 rows, but has 0 rows."
  )
})

test_that("ppa_random.data.frame - ES is a dataframe with 0 columns", {
  expect_error(
    ES |>
      dplyr::select(-dplyr::all_of(names(ES))) |>
      ppa_random(P),
    "x Assertion on ES failed.
i Must have at least 1 cols, but has 0 cols."
  )
})

test_that("ppa_random.data.frame - P is null", {
  expect_error(
    ES |>
      ppa_random(NULL),
    "x Assertion on P failed.
i Must be of type 'data.frame', not 'NULL'."
  )
})

test_that("ppa_random.data.frame - P is a dataframe with 0 rows", {
  expect_error(
    ES |>
      ppa_random(
        P |> dplyr::filter(sp1 == -1)
      ),
    "x Assertion on P failed.
i Must have at least 1 rows, but has 0 rows."
  )
})

test_that("ppa_random.data.frame - P is a dataframe with 0 columns", {
  expect_error(
    ES |>
      ppa_random(
        P |> dplyr::select(-dplyr::all_of(names(P)))
      ),
    "x Assertion on P failed.
i Must have at least 1 cols, but has 0 cols."
  )
})

test_that("ppa_random.data.frame - P is a vector and ES_idx not informed.", {
  expect_snapshot(
    ppa <- ES |>
      dplyr::select(-cell_id) |>
      dplyr::bind_cols(P) |>
      ppa_random(
        c("sp1",  "sp2", "sp3")
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_false(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})

test_that("ppa_random.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is T.", {
  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      ppa_random(
        c("sp1",  "sp2", "sp3"),
        ES_idx = "cell_id"
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_false(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})

test_that("ppa_random.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is F.", {
  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      ppa_random(
        c("sp1",  "sp2", "sp3"),
        ES_idx = "cell_id",
        remove_ES_idx = FALSE
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_true(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("cell_id", "sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("cell_id", "sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("cell_id", "sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})


test_that("ppa_random.data.frame - P is a DF and ES_idx not informed.", {
  expect_snapshot(
    ppa <- ES |>
      dplyr::select(-cell_id) |>
      ppa_random(
        P
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_false(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})

test_that("ppa_random.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is T.", {
  expect_snapshot(
    ppa <- ES |>
      ppa_random(
        P,
        ES_idx = "cell_id"
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_false(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})

test_that("ppa_random.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is F.", {
  expect_snapshot(
    ppa <- ES |>
      ppa_random(
        P,
        ES_idx = "cell_id",
        remove_ES_idx = FALSE
      )
  )
  expect_equal(
    ppa |> names(),
    c("sp1", "sp2", "sp3")
  )
  expect_true(
    "cell_id" %in% names(ppa$sp1)
  )
  expect_equal(
    ppa$sp1 |> nrow(),
    176
  )
  expect_equal(
    ppa$sp2 |> nrow(),
    156
  )
  expect_equal(
    ppa$sp3 |> nrow(),
    136
  )
  expect_equal(
    ppa$sp1 |> names(),
    c("cell_id", "sp1", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp2 |> names(),
    c("cell_id", "sp2", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
  expect_equal(
    ppa$sp3 |> names(),
    c("cell_id", "sp3", "length_km", "dist_dn_km", "dist_up_km", "catch_skm", "upland_skm",
      "dis_av_cms", "ord_stra", "bio_1", "bio_2", "bio_3", "bio_4", "bio_5",
      "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_12",
      "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
  )
})

test_that("ppa_random.data.frame - Throws 2 different errors to 3 species.", {
  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      dplyr::filter(sp1>0) |>
      dplyr::select(-c(sp1, sp2, sp3)) |>
      ppa_random(
        P |> dplyr::filter(sp1>0),
        ES_idx = "cell_id",
        remove_ES_idx = FALSE
      )
  )
  checkmate::check_names(
    names(ppa),
    type = "unique",
    identical.to = c("sp1", "sp2", "sp3")
  )
})

