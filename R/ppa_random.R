#' @title Calculate Randomly Presences and Pseudo-absences from an Enviromental Space ES and presences P
#' @param ES
#'
#' @param P
#' @param ES_idx
#'
#' @export ppa_random
#' @rdname ppa_random
ppa_random <- function(ES, P, ES_idx) {
  UseMethod("ppa_random")
}

#' @return \code{NULL}
#'
#' @rdname ppa_random
#' @method ppa_random data.frame
#' @export
ppa_random.data.frame <- function(ES, P = NULL, ES_idx = NULL) {
  return(
    generate_ppa(
      ES = ES,
      P = P,
      ES_idx = ES_idx,
      ppa_function = .ppa_random)
  )
}


#' @title .ppa_random
#'
#' @param ES
#' @param P
#' @param ES_idx
#' @param sp_name
#'
#' @return
#'
#' @keywords internal
#' @noRd
.ppa_random <- function(ES, P = NULL, ES_idx = NULL, sp_name = NULL) {
  names_ES <- names(ES) |> setdiff(c(ES_idx, sp_name))
  names_P <- names(P) |> setdiff(c(ES_idx, sp_name))

  names_both <- names_P[names_P %in% names_ES]

  ES <- ES |>
    dplyr::anti_join(P, by = names_both)

  PPA <- NULL
  set.seed(nrow(P))
  if (nrow(ES) == 0) {
    PPA <- NULL
    cli::cli_warn(
      c(
        "!" = "The size of P is equal to the size of ES.",
        "i" = "PPA can't be generated to {sp_name}."
      )
    )
  } else if (nrow(ES) > nrow(P)) {
    PPA <- dplyr::sample_n(ES, nrow(P))
  } else {
    PPA <- dplyr::sample_n(ES, nrow(P), replace = TRUE)
  }
  PPA[[sp_name]] <- 0

  PPA <- P |>
    dplyr::bind_rows(PPA)

  return(PPA)
}
