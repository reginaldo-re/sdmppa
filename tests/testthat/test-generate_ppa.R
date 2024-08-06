if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  ES <- here::here("tests", "testthat", "testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
} else {
  ES <- test_path("testdata", "ES.csv") |>
    vroom::vroom(show_col_types = FALSE)
}
P <- ES |> dplyr::select(c(sp1, sp2, sp3))
ES <- ES |> dplyr::select(-c(sp1, sp2, sp3))


## generate_ppa.data.frame

test_that("generate_ppa.data.frame - ES_idx not in ES.", {
  expect_error(
    ES |>
      generate_ppa(P, ES_idx = "foo"),
    "x Assertion on ES_idx failed.
i Must be element of set"
  )
})




test_that("generate_ppa.data.frame - P is a vector that does not match any column in ES.", {
  expect_error(
    ES |>
      dplyr::bind_cols(P) |>
      generate_ppa(c("foo")),
    "x Assertion on names"
  )
})

test_that("generate_ppa.data.frame - P is a vector that match the ES_idx column in ES.", {
  expect_error(
    ES |>
      dplyr::bind_cols(P) |>
      generate_ppa(c("cell_id"),
                   ES_idx = "cell_id"),
    "P does not contains species names."
  )
})

test_that("generate_ppa.data.frame - cell_id of P does not join any row in ES", {
  P_tmp <- P |> dplyr::bind_cols(ES |> dplyr::select(cell_id))
  P_tmp$cell_id <- 1
  expect_error(
    ES |>
      generate_ppa(
        P_tmp,
        ES_idx = "cell_id"
      ),
    "40222 rows of P can't be joined to ES by cell_id"
  )
})

test_that("generate_ppa.data.frame - P is a vector that match the ES_idx column in ES.", {
  expect_error(
    ES |>
      generate_ppa(P |> dplyr::filter(sp1 > 0)),
    "It was not possible to bind columns from ES and P"
  )
})

test_that("generate_ppa.data.frame - ES is a dataframe with 0 rows", {
  expect_error(
    ES |>
      dplyr::filter(cell_id == -1) |>
      generate_ppa(P),
    "x Assertion on ES failed.
i Must have at least 40222 rows, but has 0 rows."
  )
})

test_that("generate_ppa.data.frame - ES is a dataframe with 0 columns", {
  expect_error(
    ES |>
      dplyr::select(-dplyr::all_of(names(ES))) |>
      generate_ppa(P),
    "x Assertion on ES failed.
i Must have at least 1 cols, but has 0 cols."
  )
})

test_that("generate_ppa.data.frame - P is null", {
  expect_error(
    ES |>
      generate_ppa(NULL),
    "x Assertion on P failed.
i Must be of type 'data.frame', not 'NULL'."
  )
})

test_that("generate_ppa.data.frame - P is a dataframe with 0 rows", {
  expect_error(
    ES |>
      generate_ppa(
        P |> dplyr::filter(sp1 == -1)
      ),
    "x Assertion on P failed.
i Must have at least 1 rows, but has 0 rows"
  )
})

test_that("generate_ppa.data.frame - P is a dataframe with 0 columns", {
  expect_error(
    ES |>
      generate_ppa(
        P |> dplyr::select(-dplyr::all_of(names(P)))
      ),
    "x Assertion on P failed.
i Must have at least 1 cols, but has 0 cols"
  )
})

test_that("generate_ppa.data.frame - P is a vector and ES_idx not informed.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("..ES_idx"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("..ES_idx")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      dplyr::select(-cell_id) |>
      dplyr::bind_cols(P) |>
      generate_ppa(
        c("sp1",  "sp2", "sp3"),
        ppa_function = my_func
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

test_that("generate_ppa.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is T.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("cell_id"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("cell_id")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      generate_ppa(
        c("sp1",  "sp2", "sp3"),
        ES_idx = "cell_id",
        ppa_function = my_func
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

test_that("generate_ppa.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is F.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("cell_id"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("cell_id")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      generate_ppa(
        c("sp1",  "sp2", "sp3"),
        ES_idx = "cell_id",
        ppa_function = my_func,
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


test_that("generate_ppa.data.frame - P is a DF and ES_idx not informed.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("..ES_idx"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("..ES_idx")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      dplyr::select(-cell_id) |>
      generate_ppa(
        P,
        ppa_function = my_func
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

test_that("generate_ppa.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is T.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("cell_id"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("cell_id")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      generate_ppa(
        P,
        ES_idx = "cell_id",
        ppa_function = my_func
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

test_that("generate_ppa.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is F.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("cell_id"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("cell_id")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }

  expect_snapshot(
    ppa <- ES |>
      generate_ppa(
        P,
        ES_idx = "cell_id",
        ppa_function = my_func,
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

test_that("ppa_random.data.frame - Throws different errors to 3 species.", {
  my_func <- function(ES, P, ES_idx, sp_name) {
    tmp_P <- P |>
      dplyr::filter(!!rlang::sym(sp_name) > 0)
    PPA <- ES |>
      dplyr::anti_join(tmp_P, by = c("cell_id"))
    if (nrow(PPA) > 0) {
      PPA <- PPA |>
        dplyr::sample_n(nrow(tmp_P))
    }
    PPA[sp_name] <- 0
    tmp_P |> dplyr::left_join(ES, by = c("cell_id")) |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }


  expect_snapshot(
    ppa <- ES |>
      dplyr::bind_cols(P) |>
      dplyr::filter(sp1>0) |>
      dplyr::select(-c(sp1, sp2, sp3)) |>
      generate_ppa(
        P |> dplyr::filter(sp1>0),
        ES_idx = "cell_id",
        remove_ES_idx = FALSE,
        ppa_function = my_func
      )
  )

  checkmate::expect_names(
    names(ppa),
    type = "unique",
    identical.to = c("sp1", "sp2", "sp3")
  )
})


# ## .generate_single_ppa
#
# test_that(".generate_single_ppa.data.frame - ES is a dataframe with 0 rows", {
#   expect_error(
#     ES |>
#       dplyr::filter(cell_id == -1) |>
#       .generate_single_ppa(P),
#     "Assertion on 'ES' failed: Must have at least 1 rows, but has 0 rows."
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - ES is a dataframe with 0 columns", {
#   expect_error(
#     ES |>
#       dplyr::select(-names(ES)) |>
#       .generate_single_ppa(P),
#     "Assertion on 'ES' failed: Must have at least 1 cols, but has 0 cols."
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - P is a dataframe with 0 rows", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |> dplyr::filter(sp1 == -1)
#       ),
#     "Assertion on 'P' failed: Must have at least 1 rows, but has 0 rows."
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - ppa_function is NULL. ", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         sp_name = "sp1"
#       ),
#     "Assertion on 'ppa_function' failed: Must be a function, not 'NULL'"
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - ppa_function has invalid parameters.", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         ppa_function = \(ES, P, ES_idx) {},
#         sp_name = "sp1"
#       ),
#     "Assertion on 'ppa_function' failed: Must have formal arguments"
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - ES_idx is NULL.", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::bind_cols(ES),
#         ES_idx = NULL,
#         ppa_function = \(ES, P, ES_idx, sp_name) {}
#       ),
#     "Assertion on 'ES_idx' failed: Must be a subset of"
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - Problems in joinning ES and P using ES_idx.", {
#   testthat::expect_snapshot(
#     {
#       ES |>
#         .generate_single_ppa(
#           P |>
#             dplyr::select(sp1) |>
#             dplyr::bind_cols(ES) |>
#             dplyr::mutate(cell_id = 1),
#           ES_idx = "cell_id",
#           ppa_function = \(ES, P, ES_idx, sp_name) {},
#           sp_name = "sp1"
#         )
#     },
#     error = TRUE
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - sp_name does not match names in P. ", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         ppa_function = \(ES, P, ES_idx, sp_name) {},
#         sp_name = "sp2"
#       ),
#     "Assertion on 'sp_name' failed: Must be element of set"
#   )
# })
#
#
# test_that(".generate_single_ppa.data.frame - PPA generated is NULL.", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         ppa_function = \(ES, P, ES_idx, sp_name) {},
#         sp_name = "sp1"
#       ),
#     "Assertion on 'PPA' failed: Must be of type 'data.frame', not 'NULL'."
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - The number of Presences and pseudo-absences is not equal.", {
#   expect_error(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           return(
#             P_sp1 |>
#               dplyr::bind_rows(PA_sp1 |> head(nrow(P_sp1) - 1))
#           )
#         },
#         sp_name = "sp1"
#       ),
#     "Assertion on 'PA_size == P_size' failed: Must be TRUE."
#   )
# })
#
# test_that(".generate_single_ppa.data.frame - The number of Presences and pseudo-absences is equal.", {
#   expect_equal(
#     ES |>
#       .generate_single_ppa(
#         P |>
#           dplyr::select(sp1) |>
#           dplyr::bind_cols(ES),
#         ES_idx = "cell_id",
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           return(
#             P_sp1 |>
#               dplyr::bind_rows(PA_sp1)
#           )
#         },
#         sp_name = "sp1"
#       ),
#     P_sp1 |>
#       dplyr::bind_rows(PA_sp1)
#   )
# })
#
# ## generate_ppa.data.frame
#
# test_that("generate_ppa.data.frame - ES is a dataframe with 0 rows", {
#   expect_error(
#     ES |>
#       dplyr::filter(cell_id == -1) |>
#       generate_ppa(P),
#     "Assertion on 'ES' failed: Must have exactly 40222 rows, but has 0 rows."
#   )
# })
#
# test_that("generate_ppa.data.frame - ES is a dataframe with 0 columns", {
#   expect_error(
#     ES |>
#       dplyr::select(-names(ES)) |>
#       generate_ppa(P),
#     "Assertion on 'ES' failed: Must have at least 1 cols, but has 0 cols."
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P is null", {
#   expect_error(
#     ES |>
#       generate_ppa(NULL),
#     "Assertion on 'P' failed: Must be of type 'data.frame', not 'NULL'."
#   )
# })
#
# test_that("generate_ppa.data.frame - P is a dataframe with 0 rows", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |> dplyr::filter(sp1 == -1)
#       ),
#     "Assertion on 'ES' failed:"
#   )
# })
#
# test_that("generate_ppa.data.frame - P is a dataframe with 0 columns", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |> dplyr::select(-names(P))
#       ),
#     "Assertion on 'P' failed: Must have at least 1 cols, but has 0 cols."
#   )
# })
#
# test_that("generate_ppa.data.frame - P is a dataframe with less rows than ES", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |> head()
#       ),
#     "Assertion on 'ES' failed: Must have exactly 6 rows, but has 40222 rows."
#   )
# })
#
# test_that("generate_ppa.data.frame - P is a dataframe with more rows than ES", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |>
#           dplyr::bind_rows(P)
#       ),
#     "Assertion on 'ES' failed: Must have exactly 80444 rows, but has 40222 rows."
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has columns does not match ES, and ES_idx is not defined.", {
#   expect_equal(
#     ES |>
#       generate_ppa(
#         P,
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1 |> dplyr::mutate(.ES_idx = 1))
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2 |> dplyr::mutate(.ES_idx = 1))
#           } else {
#             return(PPA_sp3 |> dplyr::mutate(.ES_idx = 1))
#           }
#         }
#       ),
#     list(
#       "sp1" = PPA_sp1,
#       "sp2" = PPA_sp2,
#       "sp3" = PPA_sp3
#     )
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has one column matching ES, and ES_idx is not defined.", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |> dplyr::bind_cols(ES |> dplyr::select(bio_1)),
#         ppa_function = \(ES, P, ES_idx, sp_name) {}
#       ),
#     "Assertion on 'P' failed: Names must be disjunct from"
#   )
# })
#
# test_that("generate_ppa.data.frame - P has columns does not match ES, but ES_idx is defined and matches only ES.", {
#   expect_equal(
#     ES |>
#       generate_ppa(
#         P,
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "cell_id"
#       ),
#     list(
#       "sp1" = PPA_sp1 |> dplyr::select(-cell_id),
#       "sp2" = PPA_sp2 |> dplyr::select(-cell_id),
#       "sp3" = PPA_sp3 |> dplyr::select(-cell_id)
#     )
#   )
# })
#
# test_that("generate_ppa.data.frame - P has columns does not match ES, but ES_idx is defined and matches only P.", {
#   expect_error(
#     ES |>
#       dplyr::select(-cell_id) |>
#       generate_ppa(
#         P |> dplyr::bind_cols(ES |> dplyr::select(cell_id)),
#         ppa_function = \(ES, P, ES_idx, sp_name) {},
#         ES_idx = "cell_id"
#       ),
#     "Assertion on 'ES_idx' failed: Must be element of set"
#   )
# })
#
# test_that("generate_ppa.data.frame - P has one column matching ES, and ES_idx is defined and matches P and ES.", {
#   expect_equal(
#     ES |>
#       generate_ppa(
#         P |> dplyr::bind_cols(ES |> dplyr::select(cell_id)),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "cell_id"
#       ),
#     list(
#       "sp1" = PPA_sp1,
#       "sp2" = PPA_sp2,
#       "sp3" = PPA_sp3
#     )
#   )
# })
#
# test_that("generate_ppa.data.frame - P has one column matching ES, and ES_idx is defined but not matches P nor ES.", {
#   expect_error(
#     ES |>
#       generate_ppa(
#         P |> dplyr::bind_cols(ES |> dplyr::select(bio_1)),
#         ppa_function = \(ES, P, ES_idx, sp_name) {},
#         ES_idx = "foo"
#       ),
#     "Assertion on 'ES_idx' failed: Must be element of set"
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has two columns matching ES, and ES_idx is defined and matches P and ES.", {
#   testthat::expect_snapshot(
#     {
#       ES |>
#         generate_ppa(
#           P |> dplyr::bind_cols(ES |> dplyr::select(cell_id, bio_1)),
#           ppa_function = \(ES, P, ES_idx, sp_name) {
#             if (sp_name == "sp1") {
#               return(PPA_sp1)
#             } else if (sp_name == "sp2") {
#               return(PPA_sp2)
#             } else {
#               return(PPA_sp3)
#             }
#           },
#           ES_idx = "cell_id"
#         )
#     },
#     error = TRUE
#   )
# })
#
# test_that("generate_ppa.data.frame - P is a dataframe/vector with 0 rows/elements.", {
#   expect_error(
#     ES |>
#       generate_ppa(P |> dplyr::filter(sp1 == 100)),
#     "Assertion on 'ES' failed: Must have exactly 0 rows, but has 40222 rows."
#   )
#   expect_error(
#     ES |>
#       generate_ppa(list()),
#     "Assertion on 'P' failed: Must have names."
#   )
# })
#
# test_that("generate_ppa.data.frame - # P is a dataframe/vector and does not match ES.", {
#   expect_error(
#     ES |>
#       dplyr::bind_cols(P) |>
#       generate_ppa(c("foo1", "foo2", "foo3")),
#     "Assertion on 'P' failed: Names must be a subset of"
#   )
#   expect_error(
#     ES |>
#       dplyr::bind_cols(P) |>
#       generate_ppa(list("foo1", "foo2", "foo3")),
#     "Assertion on 'P' failed: Names must be a subset of"
#   )
# })
#
# test_that("generate_ppa.data.frame - P has one element matching ES, and ES_idx is not defined.", {
#   expect_equal(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp2", "sp3"))) |>
#       generate_ppa(
#         c("sp1"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1 |> dplyr::mutate(.ES_idx = 1))
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2 |> dplyr::mutate(.ES_idx = 1))
#           } else {
#             return(PPA_sp3 |> dplyr::mutate(.ES_idx = 1))
#           }
#         }
#       ),
#     list("sp1" = PPA_sp1)
#   )
# })
#
# test_that("generate_ppa.data.frame - P has one element matching ES, and ES_idx is defined and matches ES.", {
#   testthat::expect_snapshot({
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp1", "sp2", "sp3"))) |>
#       generate_ppa(
#         c("cell_id"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1 |> dplyr::mutate(.ES_idx = 1))
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2 |> dplyr::mutate(.ES_idx = 1))
#           } else {
#             return(PPA_sp3 |> dplyr::mutate(.ES_idx = 1))
#           }
#         },
#         ES_idx = "cell_id"
#       )
#   })
# })
#
# test_that("generate_ppa.data.frame - P has one element matching ES, and ES_idx is defined and matches another column in ES.", {
#   expect_equal(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp2", "sp3"))) |>
#       generate_ppa(
#         c("sp1", "cell_id"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "cell_id"
#       ),
#     list("sp1" = PPA_sp1)
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has one element matching ES, and ES_idx is defined and it does not match ES.", {
#   expect_error(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp2", "sp3"))) |>
#       generate_ppa(
#         c("sp1", "cell_id"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "foo"
#       ),
#     "Assertion on 'ES_idx' failed: Must be element of set"
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has two elements matching ES, and ES_idx is defined and matches ES.", {
#   expect_equal(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp3"))) |>
#       generate_ppa(
#         c("sp1", "sp2", "cell_id"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "cell_id"
#       ),
#     list("sp1" = PPA_sp1, "sp2" = PPA_sp2)
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has two elements matching ES, and ES_idx is not defined.", {
#   expect_equal(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp3"))) |>
#       generate_ppa(
#         c("sp1", "sp2"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1)
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2)
#           } else {
#             return(PPA_sp3)
#           }
#         },
#         ES_idx = "cell_id"
#       ),
#     list(
#       "sp1" = PPA_sp1 |> dplyr::select(-cell_id),
#       "sp2" = PPA_sp2 |> dplyr::select(-cell_id)
#     )
#   )
# })
#
#
# test_that("generate_ppa.data.frame - P has two elements matching ES, and ES_idx is not defined.", {
#   expect_equal(
#     ES |>
#       dplyr::bind_cols(P) |>
#       dplyr::select(-dplyr::all_of(c("sp3"))) |>
#       generate_ppa(
#         c("sp1", "sp2"),
#         ppa_function = \(ES, P, ES_idx, sp_name) {
#           if (sp_name == "sp1") {
#             return(PPA_sp1 |> dplyr::mutate(.ES_idx = 1))
#           } else if (sp_name == "sp2") {
#             return(PPA_sp2 |> dplyr::mutate(.ES_idx = 1))
#           } else {
#             return(PPA_sp3 |> dplyr::mutate(.ES_idx = 1))
#           }
#         }
#       ),
#     list("sp1" = PPA_sp1, "sp2" = PPA_sp2)
#   )
# })
#
# test_that("generate_ppa.data.frame - Problems in joinning ES and P using ES_idx.", {
#   testthat::expect_snapshot(
#     {
#       ES |>
#         generate_ppa(
#           P |>
#             dplyr::select(sp1) |>
#             dplyr::mutate(cell_id = 1),
#           ES_idx = "cell_id",
#           ppa_function = \(ES, P, ES_idx, sp_name) {},
#           sp_name = "sp1"
#         )
#     },
#     error = TRUE
#   )
# })
