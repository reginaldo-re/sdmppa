#' @title Generic Function to Calculate Presences and Pseudo-absences from an
#' Enviromental Space ES and presences P
#'
#' @description
#' Essa função é usada por todos os métodos de geração de pseudo-ausências. Seu
#' objetivo é validar os dados de entrada para tal geração, execuar a geração
#' para cada uma das espécies e, validar a saída grenéria para cada método de
#' geração.
#'
#' @param ES Um dataframe representando o espaço ambiental contendo as variáveis a
#' serem usadas na geração das pseudo-ausências. Esse dataframe pode conter colunas
#' representando as presenças das espécies. Cada espécie em uma coluna e cada
#' presença em uma linha denotada por valores maiores do que zero. Pode conter,
#' também,  uma coluna indicando o `ES_idx`, que é um índice para o espaço
#' geográfico representado, que pode referenciar uma grid
#' de polígonos ou um raster. Se houver `ES_idx` em **ES**, existe a opção de
#' mante-lo ou não nas presenças e pseudo-ausências geradas.
#' @param P Caso **ES** contenha colunas representando as presenças, **P** pode
#' ser um vetor com os nomes das colunas que representam as presenças das
#' espécies.
#' Caso **ES** contenha apenas o espaço ambiental, **P** é um dataframe que
#' representa as presenças das espécies. Cada espécie em uma coluna e cada
#' presença em uma linha denotada por valores maiores do que zero. **P** pode
#' conter uma coluna `ES_idx`, indicando qual linha em **P** será ligada a qual
#' linha de **ES** por meio de uma juncão. Mas, se não houver e a quantidade de
#' linhas em **P** for igual a quantidade de linhas em **ES**, ao invés de um join,
#' um `bind_cols` é usado para indicar qual linha em **P** é ligada a qual linha
#' em **ES**. Se `ES_idx` não estiver em **P** e quantidade de linhas em **P** for
#' diferente de **ES**, um erro é gerado.
#' @param ES_idx Indica o nome da coluna em **ES**, e eventualmente em **P**, é
#' usada para indicar o espaço geográfico representado em **ES** e, também, para
#' ligar **ES** a **P**. Pode ser omitido e, nesse caso, o espaço geográfico é
#' desconsiderado.
#' @param ppa_function Função que é responsável por fazer a geração das
#' presenças e pseudo-ausências. Essa função é passada para
#' `.generate_single_ppa` e deve seguir os mesmos parâmetros que
#' `.generate_single_ppa.`
#' @param remove_ES_idx Indica se a coluna que representa o índice do espaço
#' geográfico deve ou não ser mantido nas presenças e pseudo-ausências geradas.
#' @param ... Demais parâmetros passadas para a função `ppa_function`.
#'
#' @return O valor retornado é uma lista nomeada com as espécies e, em cada
#' posição da lista, um dataframe contendo as presenças e pseudo-ausências,
#' juntamente com as variáveis ambientais e, opcionalmente, `ES_idx`.
#'
#' @examples
#'
#' \dontrun{
#' a
#'}
#' @rdname generate_ppa
#' @export generate_ppa
generate_ppa <- function(ES, P, ES_idx, ppa_function, remove_ES_idx, ...) {
  UseMethod("generate_ppa")
}

#' @rdname generate_ppa
#' @method generate_ppa data.frame
#' @export
generate_ppa.data.frame <-
  function(ES,
           P = NULL,
           ES_idx = NULL,
           ppa_function = NULL,
           remove_ES_idx = TRUE,
           ...) {
    if (!is.null(ES_idx)) {
      assert_choice_cli(
        ES_idx,
        choices = names(ES),
        null.ok = TRUE
      )
    }
    if (checkmate::test_atomic_vector(P) || checkmate::test_list(P)) {
      P <- P |>
        unlist() |>
        dplyr::setdiff(ES_idx)

      names_P <- P
      assert_names_cli(
        names_P,
        subset.of = names(ES),
        .var.name = "names(P)"
      )
      if (length(names_P) == 0) {
        cli::cli_abort(
          c(
            "x" = "P does not contains species names."
          )
        )
      }
      P <- ES |>
        dplyr::select(dplyr::all_of(names_P))

      ES <- ES |>
        dplyr::select(-dplyr::all_of(names_P))
    }
    assert_data_frame_cli(
      ES,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      min.rows = nrow(P)
    )
    assert_data_frame_cli(
      P,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      min.cols = 1,
      min.rows = 1
    )

    if (!is.null(ES_idx)) {
      if (ES_idx %in% names(P)){
        tmp_P <- P |>
          dplyr::anti_join(ES, by = c(ES_idx))
        if (nrow(tmp_P) > 0) {
          cli::cli_abort(
            c(
              "x" = "{nrow(tmp_P)} row{?s} of P can't be joined to ES by {ES_idx}.",
              "i" = "Please check the P."
            )
          )
        }
        cli::cli_inform(
          c(
            "i" = "Joining ES and P by { ES_idx }."
          )
        )
        P <- ES |>
          dplyr::select(dplyr::all_of(ES_idx)) |>
          dplyr::left_join(P, by = c(ES_idx)) |>
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric),  ~ tidyr::replace_na(., 0)))
      } else {
        cli::cli_warn(
          c(
            "!" = "{ ES_idx } is present in ES but is not present in P.",
            "i" = "ES and P have the same number of rows.",
            "i" = "ES and P were bonded using their row number."
          )
        )
        P[ES_idx] <- ES[ES_idx]
      }
    } else {
      if (nrow(ES) == nrow(P)) {
        cli::cli_warn(
          c(
            "!" = "ES_idx is not informed.",
            "i" = "ES and P have the same number of rows.",
            "i" = "ES and P were bonded using their row number."
          )
        )
      } else {
        cli::cli_abort(
          c(
            "x" = "It was not possible to bind columns from ES and P.",
            "i" = "Please inform an ES_idx column valid to ES and P."
          )
        )
      }
      ES_idx <- "..ES_idx"
      ES$..ES_idx <- seq(1, nrow(ES))
      P$..ES_idx <- seq(1, nrow(ES))
      remove_ES_idx <- TRUE
    }
    assert_names_cli(
      names(ES),
      disjunct.from = names(P) |> dplyr::setdiff(ES_idx),
      .var.name = "names(ES)"
    )
    safe_generate_single_ppa <- purrr::safely(
      \(sp_name) {
        tmp_PPA <- ES |>
          .generate_single_ppa(
            P = P |> dplyr::select(dplyr::all_of(c(sp_name, ES_idx))),
            ES_idx = ES_idx,
            sp_name = sp_name,
            ppa_function = ppa_function,
            ...
          )

        if (remove_ES_idx) {
          tmp_PPA <- tmp_PPA |>
            dplyr::select(-dplyr::all_of(ES_idx))
        }
        return(tmp_PPA)
      }
    )
    names_P <- names(P) |> dplyr::setdiff(ES_idx)
    PPA <- names_P |>
      purrr::set_names(names_P) |>
      purrr::map(safe_generate_single_ppa)

    msg_errors <- PPA |>
      purrr::map("error") |>
      purrr::map(
        \(x) {
          paste(x$message, x$body)
        }
      ) |>
      unlist()

    if (length(msg_errors) > 0){
      msg_errors <- msg_errors |>
        purrr::imap_chr(
          \(m, idx) {
            glue::glue("({ names(msg_errors[idx]) }) : { m } ") |>
              fmt_bullet_cli(cli_bullet = "i")
          }
        ) |> stats::setNames(rep("i", length(msg_errors)))
      msg_errors <- c(
        "!" = "Problems occurs to process some species.",
        msg_errors
      )
      cli::cli_warn(msg_errors)
    }
    PPA <- PPA |>
      purrr::map("result")

    assert_names_cli(
      names(PPA) |> dplyr::setdiff(ES_idx),
      identical.to = names_P
    )

    return(PPA)
  }

#' @title .generate_single_ppa
#'
#' @description
#'
#' Produz como resultado um dataframe com as presenças e as pseudo-ausências
#' geradas pela função que está em `ppa_function`.
#'
#' @param ES é o espaço ambiental composto pelas variáveis ambientais.
#' Representado sempre por um dataframe, deve conter uma
#' coluna que que é um índice (`ES_idx`). Esse índice pode referenciar uma grid
#' de polígonos ou um raster que representa o espaço geográfico. Ou, então,
#' apenas um índice de ligação entre **P** e **ES**. Essa coluna
#' deve identificar univocamente cada célula da grid ou raster.
#' @param P é o espaço ambiental das presenças de uma dada espécie.
#' Representado por um dataframe, deve conter um subconjunto das variáveis de
#' **ES**, e também um índice, pelo qual será feita a junção com **ES**. Deve
#' conter, além do índice, uma coluna com o nome da espécie (`sp_name`) cujos
#' valores representam as presenças. Qualquer valor acima de zero é considerado
#' uma presença. **P** e **ES** sofrerão uma junção por meio de `ES_idx`, de maneira
#' que todas linhas de **P** tenham correspondência em **ES**, e **ES** tenha linhas
#' suficientes para serem geradas as pseudo-ausências.
#' @param ES_idx armazena o nome da coluna de **ES** que deve ser usada como
#' índice.
#' @param sp_name armazena o nome da coluna de **ES** que representa a espécie.
#' @param ppa_function função que será invocada para efetivamente gera as
#' presenças e pseudo-ausências.
#'
#' @return Retorna um dataframe com o espaço ambiental de **ES** que representa
#' as presenças (**P**) e as pseudo-ausências geradas (`PA`). O número de
#' presenças é sempre o mesmo número de pseudo-ausências.
#'
#' @details Essa função valida a entrada **P**, **ES**, `ES_idx`, `sp_name` e
#' `ppa_function` invoca a função de geração especifica que está em
#' `ppa_function` e checa a saida produzida, que chamamos de `PPA`, representada
#' por um dataframe contendo em mesmo número as presenças e as pseudo-ausências.
#'
#' @rdname generate_single_ppa
#' @keywords internal
.generate_single_ppa <-
  function(ES = NULL,
           P = NULL,
           ES_idx = NULL,
           sp_name = NULL,
           ppa_function = NULL,
           ...) {
    assert_data_frame_cli(
      P,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      ncols = 2,
      nrows = nrow(ES),

    )
    assert_choice_cli(
      ES_idx,
      choices = names(ES) |> dplyr::intersect(names(P)),
      null.ok = FALSE
    )
    assert_names_cli(
      names(P),
      identical.to = c(sp_name, ES_idx),
      type = "unique",
      .var.name = "names(P)"
    )
    assert_function_cli(
      ppa_function,
      c("ES", "P", "ES_idx", "sp_name"),
      null.ok = FALSE
    )

    P_tmp <- P |>
      dplyr::left_join(ES, by = c(ES_idx))

    if (nrow(P_tmp) != nrow(P)) {
      cli::cli_abort(
        c(
          "x" = "{nrow(P) - nrow(P_tmp)} row{?s} of P can't be joined to ES by {ES_idx}.",
          "i" = "Please check the P."
        )
      )
    }

    PPA <- ES |> ppa_function(
      P,
      ES_idx,
      sp_name,
      ...)

    assert_data_frame_cli(
      PPA,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE
    )

    assert_names_cli(
      names(PPA),
      permutation.of = c(names(P), names(ES))
    )

    P_size <- PPA |>
      dplyr::filter(.data[[sp_name]] > 0) |>
      nrow()
    assert_int_cli(
      P_size,
      na.ok = FALSE,
      null.ok = FALSE,
      lower = nrow(P |> dplyr::filter(.data[[sp_name]] > 0)),
      upper = nrow(P |> dplyr::filter(.data[[sp_name]] > 0)),
      .var.name = "nrow(P)"
    )

    PA_size <- PPA |>
      dplyr::filter(.data[[sp_name]] == 0) |>
      nrow()
    assert_int_cli(
      PA_size,
      na.ok = FALSE,
      null.ok = FALSE,
      lower = P_size,
      upper = P_size,
      .var.name = "nrow(PA)"
    )
    if (!is.null(ES_idx)) {
      assert_names_cli(
        names(PPA[1:2]),
        identical.to = c(ES_idx, sp_name),
      )
    } else {
      assert_names_cli(
        names(PPA[1:1]),
        identical.to = c(sp_name)
      )
    }

    return(PPA)
}
