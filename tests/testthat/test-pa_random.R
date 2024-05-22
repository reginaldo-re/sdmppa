P <- test_path("testdata", "predictors.csv") |>
  vroom::vroom(show_col_types = FALSE)
PP <- test_path("testdata", "argncts_ln.csv") |>
  vroom::vroom(show_col_types = FALSE)
PP_predictors <- PP |>
  dplyr::select(-argncts_ln)


test_that(".pa_random - Test invalid P", {
  expect_error(
    NULL |> .pa_random(PP_predictors),
    "Assertion on 'P' failed: Must be of type 'data.frame', not 'NULL'."
  )

  expect_error(
    matrix(nrow = 0, ncol = ncol(P)) |>
      data.frame() |>
      setNames(P) |>
      .pa_random(PP_predictors),
    "Assertion on 'P' failed: Must have at least 1 rows, but has 0 rows."
  )
})

test_that(".pa_random - Test invalid PP", {
  expect_error(
    P |>
      .pa_random(NULL),
    "Assertion on 'PP' failed: Must be of type 'data.frame', not 'NULL'."
  )

  expect_error(
    P |>
      .pa_random(
        data.frame(matrix(nrow = 0, ncol = ncol(PP_predictors))) |>
          setNames(names(PP_predictors))
      ),
    "Assertion on 'PP' failed: Must have at least 1 rows, but has 0 rows."
  )
})

test_that(".pa_random - Test if attributes of PP not in P.", {
  expect_error(
    P |>
      dplyr::select(c(1)) |>
      dplyr::rename("foo" = 1) |>
      .pa_random(PP_predictors),
    "There are no PP atributes in P!"
  )
})


test_that(".pa_random - Test success case, one less variable", {
  PAP <- P |>
    .pa_random(PP_predictors |> dplyr::select(-c(bio_1)))

  checkmate::expect_data_frame(
    PAP,
    any.missing = FALSE,
    all.missing = FALSE,
    nrows = nrow(PP_predictors),
    ncols = ncol(P),
    null.ok = FALSE
  )
  expect_equal(
    PAP |> dplyr::setdiff(PP_predictors) |> nrow(),
    nrow(PP)
  )

  expect_equal(
    PAP |> dplyr::setdiff(P) |> nrow(),
    0
  )
})

#--- pa_random.data.frame

test_that("pa_random.data.frame - Test invalid PP", {
  expect_error(
    P |>
      .pa_random(
        data.frame(matrix(nrow = 0, ncol = ncol(PP_predictors))) |>
          setNames(PP_predictors)
      ),
    "Assertion on 'PP' failed: Must have at least 1 rows, but has 0 rows."
  )

  expect_error(
    P |> .pa_random(PP_predictors |> dplyr::bind_rows(P)),
    "Assertion on 'PP' failed: Must have at most"
  )
  expect_error(
    P |> pa_random(list("cell_id" = c(P$cell_id, c(1)))),
    "failed: Must have length"
  )
  expect_error(
    P |> pa_random(list("cell_id" = c(PP_predictors$cell_id, c(NA)))),
    "failed: Contains missing values"
  )
  expect_error(
    P |> pa_random(list("cell_id" = NULL)),
    "Assertion on 'PP' failed: Must be of type 'vector', not 'NULL'."
  )
  expect_error(
    P |> pa_random(list("cell_id_not_in_P" = PP_predictors$cell_id)),
    "P does not contains PP variable name!"
  )
})


test_that(
  "pa_random.data.frame - Test success case, when intersection between
PAP and PP (dataframe) is null, |PAP|==|PP|, and PAP in PP",
  {
    PAP <- P |> pa_random(PP_predictors)

    expect_equal(
      PAP |> dplyr::setdiff(PP_predictors) |> nrow(),
      nrow(PP)
    )
    expect_equal(
      PAP |> dplyr::setdiff(P) |> nrow(),
      0
    )
  }
)


test_that("pa_random.data.frame - Inconsistent PP or list in PP", {
  expect_null(
    P |>
      pa_random(list("cell_id" = c(PP_predictors$cell_id), "foo" = c(1))) |>
      purrr::pluck(2)
  )
  expect_true(
    P |>
      pa_random(list("cell_id" = c(PP_predictors$cell_id), "foo" = c(1))) |>
      purrr::pluck(1) |>
      checkmate::test_vector()
  )
  expect_error(
    P |> pa_random(NULL),
    "PP must be a dataframe"
  )
})


test_that(
  "pa_random.data.frame - Test success case, when intersection between
PAP and PP (list) is null, |PAP|==|PP|, and PAP in PP",
  {
    expect_warning(
      P |> pa_random(list("cell_id" = c(PP_predictors$cell_id, c(1)))),
      "from PP does not match"
    )

    PAP <- P |>
      pa_random(list("cell_id" = c(PP_predictors$cell_id)))

    expect_contains(P$cell_id, PAP$cell_id)
    expect_true(!any(PAP$cell_id %in% PP$cell_id))
  }
)

test_that("pa_random.data.frame - Test a list of dataframes and named lists", {
  PAP <- P |>
    pa_random(
      list(
        PP_predictors |> dplyr::select(-c(bio_1)),
        PP_predictors,
        "cell_id" = PP_predictors$cell_id,
        list("cell_id" = PP_predictors$cell_id),
        c(1, 2),
        list(1, 2),
        "cell_id" = PP_predictors
      )
    )

  checkmate::expect_data_frame(
    PAP[[1]],
    any.missing = FALSE,
    all.missing = FALSE,
    nrows = nrow(PP_predictors),
    ncols = ncol(P),
    null.ok = FALSE
  )
  checkmate::expect_data_frame(
    PAP[[2]],
    any.missing = FALSE,
    all.missing = FALSE,
    nrows = nrow(PP_predictors),
    ncols = ncol(P),
    null.ok = FALSE
  )
  expect_true(
    checkmate::test_vector(
      PAP[[3]],
      strict = TRUE,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      len = nrow(PP)
    )
  )

  expect_equal(
    PAP[[1]] |> dplyr::setdiff(PP_predictors) |> nrow(),
    nrow(PP)
  )
  expect_equal(
    PAP[[1]] |> dplyr::setdiff(P) |> nrow(),
    0
  )
  expect_equal(
    PAP[[2]] |> dplyr::setdiff(PP_predictors) |> nrow(),
    nrow(PP)
  )
  expect_equal(
    PAP[[2]] |> dplyr::setdiff(P) |> nrow(),
    0
  )
  expect_equal(
    PAP[[3]] |> dplyr::setdiff(PP_predictors$cell_id) |> length(),
    nrow(PP)
  )
  expect_equal(
    PAP[[3]] |> dplyr::setdiff(P$cell_id) |> length(),
    0
  )

  expect_null(PAP |> purrr::pluck(5))
  expect_null(PAP |> purrr::pluck(6))
  checkmate::expect_data_frame(
    PAP[[7]],
    any.missing = FALSE,
    all.missing = FALSE,
    nrows = nrow(PP_predictors),
    ncols = ncol(P),
    null.ok = FALSE
  )
})
