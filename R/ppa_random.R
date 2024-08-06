#' @title Calculate Randomly Presences and Pseudo-absences from an Enviromental Space ES and Presences P
#'
#' @param ES A dataframe representing an environmental space
#' @param P A dataframe representing presences
#' @param ES_idx An index to geographical space
#' @param remove_ES_idx A flag indicating whether `ES_idx` must be removed in final
#' result.
#'
#' @export ppa_random
#' @rdname ppa_random
ppa_random <- function(ES, P, ES_idx, remove_ES_idx) {
  UseMethod("ppa_random")
}

#' @rdname ppa_random
#' @method ppa_random data.frame
#' @export
ppa_random.data.frame <- function(ES, P = NULL, ES_idx = NULL, remove_ES_idx = T) {
  return(
    generate_ppa(
      ES = ES,
      P = P,
      ES_idx = ES_idx,
      ppa_function = .ppa_random,
      remove_ES_idx = remove_ES_idx)
  )
}

#' @keywords internal
#' @noRd
.ppa_random <- function(ES= NULL, P = NULL, ES_idx = NULL, sp_name = NULL) {
  P <- P |>
    dplyr::filter(!!rlang::sym(sp_name) > 0)

  P_tmp <- P |>
    dplyr::left_join(ES, by = ES_idx)


  if (nrow(P_tmp) != nrow(P)) {
    cli::cli_abort(
      c(
        "x" = "{nrow(P) - nrow(P_tmp)} row{?s} of P can't be joined to ES by {ES_idx}.",
        "i" = "Please check the P."
      )
    )
  } else {
    P <- P_tmp
  }

  names_ES <- names(ES) |> setdiff(c(ES_idx, sp_name))
  names_P <- names(P) |> setdiff(c(ES_idx, sp_name))

  names_both <- names_P[names_P %in% names_ES]

  ES <- ES |>
    dplyr::anti_join(P, by = names_both)

  PPA <- NULL
  set.seed(nrow(P))
  if (nrow(ES) == 0) {
    PPA <- NULL
    cli::cli_abort(
      c(
        "x" = "There are no suficient points in ES to generate PPA.",
        "i" = "PPA can't be generated to {sp_name}."
      )
    )
  } else if (nrow(ES) > nrow(P)) {
    PPA <- dplyr::sample_n(ES, nrow(P))
  } else {
    PPA <- dplyr::sample_n(ES, nrow(P), replace = TRUE)
  }

  if (!is.null(PPA)) {
    PPA[[sp_name]] <- 0

    PPA <- P |>
      dplyr::bind_rows(PPA) |>
      dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name)))
  }
  assert_data_frame_cli(
   PPA,
   any.missing = FALSE,
   nrows = nrow(P) * 2,
   null.ok = FALSE
  )

  return(PPA)
}
