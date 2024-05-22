#' Calculate pseudo-absences from an enviromental space P and presences PP
#'
#' @description
#' `pa_random` calculates a set of pseudo-absences points (PAP) **randomly**
#' drawn from the enviromental space denoted by P.
#'
#' @param P A set representing the environmental space.
#'
#' @param PP A set representing de presence points.
#'
#' @details
#'
#' In fact, the data type of parameter **P** is not a `set`, but a `dataframe`.
#' Similarly as **P**, **PP** is not really a `set`, It must be a `dataframe`
#' or a `list`. Points in **PP** represent presence points that wil be removed
#' from **P**, once it is necessary to reduce **P** so that **PAP** and **PP**
#' keep disjoint. Therefore, **PP** can be:
#'  1. A `dataframe`, such that at least a name of one column in **P**  must
#'  match **PP**.
#'  2. A named list, such that the name of the list must match a column name in
#'  **P**, as an index. So,
#'  the values in the named list must reference rows in **P** that can be joined
#'  using the name of the list.
#'  3. A list containing both dataframes (as in 1) and named lists (as in 2).
#'
#' How is the PPA computed?
#'
#' When a `dataframe` is passed to **PP**, all rows in *P* whose column names
#' match *PP* are compared and removed if they are equal. This reduces the
#' environmental space in *P* to such that It does not contain *PP*, and avoids
#' common points in **PP** and **PAP**. There are two choices when a list is
#' passed: the list is an index referencing rows in *P*, and the name of the
#' list must match a column in **P**; or, it is an unnamed list containing data
#' frames and other inner named lists representing several sets **PP**. In both
#' cases, a `dataframe` or a named list, **PP** should match at least one column
#' in *P*.
#'
#' @returns
#'
#' The output of the function (PAP)  changes according to the three cases
#' described:
#'  - If a `dataframe` or a named list is passed as parameter, both representing
#'  a single **PP**, the value returned is also a dataframe (PAP);
#'  - If a list representing several different **PP** containing data frames or
#'  other named lists is passed as a parameter,  the returned value is a list of
#'  data frames, one for each **PAP**.
#'
#' @examples
#' P <- dplyr::tibble(
#'   cell_id = c(
#'     75000, 81017, 81374, 83506, 83508,
#'     84360, 84783, 84785, 86047, 86468
#'    ),
#'   bio_1 = c(
#'     25.24835, 26.47237, 25.09730, 25.28250, 25.25749,
#'     25.43329, 25.33722, 25.36212, 25.50991, 25.84756
#'    ),
#'   bio_2 = c(
#'     12.365706, 10.976032, 11.488524, 11.492427, 11.356132,
#'     11.477320, 11.125202, 11.271169, 10.878525, 10.985706
#'    ),
#'   bio_3 = c(
#'     68.53027, 66.84972, 70.87223, 70.63369, 70.99750,
#'     70.81410, 70.95460, 71.04424, 71.87370, 72.48579
#'    ),
#'   bio_4 = c(
#'     48.37795, 54.44825, 42.10769, 42.21368, 40.86964,
#'     41.39428, 41.72614, 41.17366, 44.82830, 46.00434
#'    )
#' )
#' PP1 <- dplyr::tibble(
#'   cell_id = c(75000, 81017, 84360, 86468),
#'   bio_1 = c(25.24835, 26.47237, 25.43329, 25.84756),
#'   bio_4 = c(48.37795, 54.44825, 41.39428, 46.00434)
#' )
#'
#' PAP <- P|> pa_random(PP1)
#' PAP <- P|> pa_random(list("cell_id"=PP1$cell_id))
#' PAP <- P|> pa_random(list(PP1, PP1, PP1))
#' PAP <- P|> pa_random(
#'   list(
#'     list("cell_id"=PP1$cell_id),
#'     list("cell_id"=PP1$cell_id),
#'     list("cell_id"=PP1$cell_id)
#'   )
#' )
#'
#' @export
pa_random <- function(P, PP) {
  UseMethod("pa_random")
}


#' @export
pa_random.data.frame <- function(P, PP = NULL) {
  if (checkmate::test_data_frame(PP)) {
    return(.pa_random_df(P, PP))
  } else if (checkmate::test_list(PP)) {
    if (length(PP) == 1) {
      return(.pa_random_list(P, PP))
    } else {
      return(.pa_random_df_list(P, PP))
    }
  }

  cli::cli_abort(
    c(
      "x",
      "PP must be a dataframe (or a list of dataframes), or a list of ids stored
      in a column of P (or a list os lists)!"
    )
  )
}

.pa_random_df <- function(P, PP) {
  checkmate::assert_data_frame(
    PP,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    min.rows = 1,
    max.rows = nrow(P)
  )
  return(.pa_random(P, PP))
}

.pa_random_list <- function(P, PP) {
  checkmate::assert_vector(
    PP[[1]],
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    max.len = nrow(P),
    strict = TRUE,
    .var.name = "PP"
  )
  id <- names(PP)[1]
  if (is.null(id) || !(id %in% names(P))) {
    cli::cli_abort(c("x", "P does not contains PP variable name!"))
  }
  PP_joined <- P |>
    dplyr::filter(!!rlang::sym(id) %in% PP[[id]])

  if (length(PP[[id]]) > nrow(PP_joined)) {
    PP_dif_elements <- setdiff(PP[[id]], PP_joined[[id]])
    PP_dif_len <- length(PP_dif_elements)
    cli::cli_warn(
      c(
        "The attribute {.var {id}} from PP does not match:",
        "i" = "There {?is/are} {PP_dif_len} element{?s} left.",
        "i" = "{id}: {PP_dif_elements}."
      )
    )
  }
  PAP <- dplyr::lst(!!id := .pa_random(P, PP_joined) |> purrr::pluck(id))
  return(PAP)
}

.pa_random_df_list <- function(P, PP) {
  PP_list <- PP |>
    purrr::imap(\(x, name_x) {
      PPA_iter <- NULL
      name_x <- as.character(name_x)
      if (checkmate::test_data_frame(x)) {
        possibly_pa_random <- purrr::possibly(
          .f = .pa_random_df,
          otherwise = NULL
        )
        PPA_iter <- possibly_pa_random(P, x)
      } else {
        if (name_x %in% colnames(P) &&
              checkmate::test_vector(x, strict = TRUE)) {
          possibly_pa_random <- purrr::possibly(
            .f = .pa_random_list,
            otherwise = NULL
          )
          PPA_iter <-
            possibly_pa_random(P, dplyr::lst(!!name_x := x)) |>
            purrr::pluck(1)
        }
      }
      return(PPA_iter)
    })
  return(PP_list)
}

.pa_random <- function(P, PP = NULL) {
  checkmate::assert_data_frame(
    P,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    min.rows = 1
  )
  checkmate::assert_data_frame(
    PP,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    min.rows = 1,
    max.rows = nrow(P)
  )


  names_P <- names(P)
  names_PP <- names(PP)
  names_both <- names_PP[names_PP %in% names_P]

  if (length(names_both) == 0) {
    cli::cli_abort(
      c(
        "x",
        "There are no PP atributes in P!"
      )
    )
  }

  P <- P |>
    dplyr::anti_join(PP, by = names_both)

  set.seed(nrow(PP))
  PAP <- dplyr::sample_n(P, nrow(PP))

  checkmate::assert_data_frame(
    PAP,
    any.missing = FALSE,
    nrows = nrow(PP),
    null.ok = FALSE
  )
  return(PAP)
}
