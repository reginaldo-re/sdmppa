#' @title Generic Function to Calculate Presences and Pseudo-absences from an
#' Enviromental Space ES and presences P
#'
#' @param ES Espaço ambiental contendo cell_id ou não e, também, as presenças
#' das espécies ou não. Deve obrigatoriamente conter as variáveis ambientais.
#' @param P
#' @param ES_idx
#' @param ppa_function
#'
#' @return
#' @export generate_ppa
#' @rdname generate_ppa
#'
#' @examples
generate_ppa <- function(ES, P, ES_idx, ppa_function, ...) {
  UseMethod("generate_ppa")
}


#' @return \code{NULL}
#'
#' @rdname generate_ppa
#' @method generate_ppa data.frame
#' @export
generate_ppa.data.frame <-
  function(ES,
           P = NULL,
           ES_idx = NULL,
           ppa_function = NULL,
           ...) {
    if (checkmate::test_atomic_vector(P) || checkmate::test_list(P)) {
      P <- P |>
        unlist()
      checkmate::assert_names(
        P,
        subset.of = names(ES)
      )
      names_P <- P
      P <- ES |>
        dplyr::select(dplyr::all_of(names_P))

      ES <- ES |>
        dplyr::select(-dplyr::all_of(names_P |> dplyr::setdiff(ES_idx)))
    }
    checkmate::assert_data_frame(
      ES,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      nrow = nrow(P)
    )
    checkmate::assert_data_frame(
      P,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      nrow = nrow(ES)
    )
    remove_ES_idx <- FALSE
    if (is.null(ES_idx)) {
      checkmate::assert_names(names(P),
                              disjunct.from = names(ES),
                              .var.name = "P")
      remove_ES_idx <- TRUE
      ES_idx <- ".ES_idx"
      ES[ES_idx] <- seq_len(nrow(ES))
      P[ES_idx] <- ES[ES_idx]
    } else {
      checkmate::assert_choice(ES_idx,
                               choices = names(ES),
                               null.ok = FALSE)
      names_both <- names(P) |> intersect(names(ES))
      if (checkmate::test_string(names_both)) {
        checkmate::check_names(names_both,
                               identical.to = ES_idx)
      } else if (checkmate::test_atomic_vector(names_both, min.len = 1)) {
        cli::cli_abort(
          c("x" = "There exists more than one column matching P and ES.",
            "i" = "{length(names_both)} column{?s}: {names_both}.")
        )
      }

      if (!(ES_idx %in% names(P))) {
        remove_ES_idx <- TRUE
        P[ES_idx] <- ES[ES_idx]
      }
    }
    checkmate::check_names(
      names(ES) |> intersect(names(P)),
      identical.to = ES_idx
    )

    joined_ES_P <- ES |>
      dplyr::inner_join(P, by = c(ES_idx))

    if (nrow(joined_ES_P) != nrow(ES)) {
      nrow_diff <- nrow(ES) - nrow(joined_ES_P)
      cli::cli_abort(
        c(
          "x" = "{nrow_diff} row{?s} of P can't be joined to ES by {ES_idx}.",
          "i" = "Please check the P."
        )
      )
    }

    names_P <- names(P) |>
      setdiff(ES_idx)
    if (length(names_P) == 0) {
      cli::cli_warn(
        c(
          "x" = "There are not variables representing enviromental space in P.",
          "i" = "Please check the P and ES."
        )
      )
    }


    PPA <- names_P |>
      purrr::set_names(names_P) |>
      purrr::map(\(sp_name) {
        tmp_PPA <- P |>
          dplyr::filter(.data[[sp_name]] > 0) |>
          dplyr::select(dplyr::all_of(c(sp_name, ES_idx))) |>
          dplyr::left_join(ES, by = ES_idx)

        tmp_PPA <- ES |>
          .generate_single_ppa(tmp_PPA, ES_idx, sp_name, ppa_function, ...)

        if (remove_ES_idx) {
          tmp_PPA <- tmp_PPA |>
            dplyr::select(-dplyr::all_of(c(ES_idx)))
        }
        return(tmp_PPA)
      })

    checkmate::check_names(names(PPA),
                           identical.to = names_P)

    return(PPA)
  }

#' @title .generate_single_ppa
#'
#' Produz como resultado um dataframe comas presenças e as pseudo-ausências
#' geradas pela função que está em `ppa_function`.
#'
#' @param ES é o espaço ambiental composto pelas variáveis ambientais ou
#' outras variáveis. Representado sempre por um dataframe, deve conter uma
#' coluna que que é um índice para células de uma grid ou um raster. Essa coluna
#' deve identificar univocamente cada célula da grid ou raster.
#' @param P é o espaço ambiental das presenças de uma dada espécie.
#' Representado por um dataframe, deve conter um subconjunto das variáveis de
#' `ES`, e também um índice, pelo qual será feita a junção com `ES`. Deve
#' conter, além do índice, uma coluna com o nome da espécie (`sp_name`) cujos
#' valores representam as presenças. Qualquer valor acima de zero é considerado
#' uma presença. `ES` e `P` devem ter a mesma quantidade de linhas.
#' @param ES_idx armazena o nome da coluna de `ES` que deve ser usada como
#' índice.
#' @param sp_name armazena o nome da coluna de `ES` que representa a espécie.
#' @param ppa_function função que será invocada para efetivamente gera as
#' presenças e pseudo-ausências.
#'
#' @return Retorna um dataframe com o espaço ambiental de `ES` que representa
#' as presenças (`P`) e as pseudo-ausências geradas (`PA`). O número de
#' presenças é sempre o mesmo número de pseudo-ausências.
#'
#' @details Essa função valida a entrada `P`, `ES`, `ES_idx`, `sp_name` e
#' `ppa_function` invoca a função de geração especifica que está em
#' `ppa_function` e checa a saida produzida, que chamamos de `PPA`, representada
#' por um dataframe contendo em mesmo número as presenças e as pseudo-ausências.
#'
#' @keywords internal
#' @noRd
.generate_single_ppa <-
  function(ES = NULL,
           P = NULL,
           ES_idx = NULL,
           sp_name = NULL,
           ppa_function = NULL,
           ...) {
    checkmate::assert_data_frame(
      ES,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      min.rows = 1
    )
    checkmate::assert_data_frame(
      P,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      min.rows = 1
    )
    checkmate::assert_choice(ES_idx,
                             choices = names(ES),
                             null.ok = FALSE)
    checkmate::assert_choice(sp_name,
                             choices = names(P) |> setdiff(names(ES)),
                             null.ok = FALSE)
    checkmate::assert_names(names(P) |> setdiff(names(ES)),
                            identical.to = sp_name,
                            type = "unique")
    checkmate::assert_function(ppa_function,
                               c("ES", "P", "ES_idx", "sp_name"),
                               null.ok = FALSE)

    joined_ES_P <- ES |>
      dplyr::inner_join(P, by = c(ES_idx))

    if (nrow(joined_ES_P) != nrow(P)) {
      nrow_diff <- nrow(P) - nrow(joined_ES_P)
      cli::cli_abort(
        c(
          "x" = "{nrow_diff} row{?s} of P can't be joined to ES by {ES_idx}.",
          "i" = "Please check P.",
        )
      )
    }

    PPA <- ppa_function(ES, P, ES_idx, sp_name)

    checkmate::assert_data_frame(
      PPA,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE
    )

    checkmate::assert_names(names(PPA),
                            permutation.of = names(P))

    P_size <- PPA |>
      dplyr::filter(.data[[sp_name]] > 0) |>
      nrow()
    checkmate::assert_true(
      P_size == nrow(P |> dplyr::filter(.data[[sp_name]] > 0))
    )
    PA_size <- PPA |>
      dplyr::filter(.data[[sp_name]] == 0) |>
      nrow()
    checkmate::assert_true(PA_size == P_size)

    return(PPA)
  }
