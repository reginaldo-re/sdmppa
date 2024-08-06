#' @title Calcula as Presenças e Pseudo-Ausências usando Clusterização sobre o
#' Espaço Ambiental ES e Presenças P
#'
#' @description
#' `ppa_cluster` calculates a set of pseudo-absences points (PA) **randomly**
#' drawn from the enviromental space denoted by ES.
#'
#' @param ES A set representing the environmental space.
#' @param num_clusters Clusters
#' @param P Presences
#' @param ES_idx  Index
#' @param perc_pres  Removing
#' @param remove_ES_idx Removing idx
#'
#' @details
#' Some details
#'
#' @returns
#'
#' PPA
#'
#' @examples
#' P <- dplyr::tibble(
#'   cell_id = c(
#'     75000, 81017, 81374, 83506, 83508,
#'     84360, 84783, 84785, 86047, 86468
#'   ),
#'   bio_1 = c(
#'     25.24835, 26.47237, 25.09730, 25.28250, 25.25749,
#'     25.43329, 25.33722, 25.36212, 25.50991, 25.84756
#'   ),
#'   bio_2 = c(
#'     12.365706, 10.976032, 11.488524, 11.492427, 11.356132,
#'     11.477320, 11.125202, 11.271169, 10.878525, 10.985706
#'   ),
#'   bio_3 = c(
#'     68.53027, 66.84972, 70.87223, 70.63369, 70.99750,
#'     70.81410, 70.95460, 71.04424, 71.87370, 72.48579
#'   ),
#'   bio_4 = c(
#'     48.37795, 54.44825, 42.10769, 42.21368, 40.86964,
#'     41.39428, 41.72614, 41.17366, 44.82830, 46.00434
#'   ),
#'   sp1 = c(0,0,0,1,1,1,0,1,0,0)
#' )
#'
#' PPA <- P |> ppa_cluster(c("sp1"))
#'
#' @export ppa_cluster
#' @rdname ppa_cluster
ppa_cluster <- function(ES, P, ES_idx, remove_ES_idx, num_clusters, perc_pres) {
  UseMethod("ppa_cluster")
}

#'
#' @rdname ppa_cluster
#' @method ppa_cluster data.frame
#' @export
ppa_cluster.data.frame <- function(ES, P = NULL, ES_idx = NULL,
                                   remove_ES_idx = TRUE,
                                   num_clusters = 0, perc_pres = 10) {
    return(
      generate_ppa(
        ES = ES,
        P = P,
        ES_idx = ES_idx,
        remove_ES_idx = remove_ES_idx,
        ppa_function = .ppa_cluster,
        num_clusters = num_clusters,
        perc_pres = perc_pres
        )
      )
  }

.ppa_cluster <- function(ES = NULL, P = NULL, ES_idx = NULL, sp_name = NULL,
                         num_clusters = 0,
                         perc_pres = 10) {
  P <- P |>
    dplyr::filter(!!rlang::sym(sp_name) > 0)

  if (num_clusters == 0) {
    num_clusters <- max(round(nrow(P) * perc_pres / 100), 1)
  }

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

  names_ES <- names(ES)
  names_P <- names(P)
  names_both <- names_P[names_P %in% names_ES] |>
    dplyr::setdiff(ES_idx)

  ES_joined <- ES |>
    dplyr::left_join(P |> dplyr::select(-dplyr::all_of(ES_idx)), by = names_both) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with(sp_name), ~ ifelse(is.na(.x), 0, .x)))

  if (ES_joined[[sp_name]] |> purrr::discard(is.na) |> length() < nrow(P)) {
    points_not_matched <- ES_joined[[sp_name]] |>
      purrr::discard(is.na) |>
      sum()
    cli::cli_abort(
      c("x" = "Something goes wrong joining ES to P!",
        "i" = "{points_not_matched} points in P does not matches ES using variables that compose the enviromental space.")
    )
  }

  set.seed(nrow(P))
  clusters <- stats::kmeans(ES |> dplyr::select(-dplyr::all_of(ES_idx)), centers = num_clusters)
  ES_joined$cluster <- clusters$cluster

  clusters_details <- ES_joined |>
    dplyr::group_by(.data$cluster) |>
    dplyr::summarise(
      cluster_size = dplyr::n(),
      sample_size = (dplyr::n() / nrow(ES_joined) * nrow(P)) |> round()
    )

  diff_calc <- nrow(P) - (clusters_details$sample_size |> sum())
  if (diff_calc != 0) {
    clusters_details <- clusters_details |>
      dplyr::arrange(dplyr::desc(.data$cluster_size),
                     dplyr::desc(.data$sample_size)) |>
      dplyr::mutate(
        sample_size = ifelse(
          dplyr::row_number() == 1,
          .data$sample_size + diff_calc,
          .data$sample_size
        )
      )
  }

  ES_joined <- ES_joined |>
    dplyr::filter(.data[[sp_name]] == 0)
  if (nrow(ES_joined)==0) {
    cli::cli_abort(
      c("x" = "Something goes wrong!",
        "i" = "There are no suficient points in ES to generate PPA.")
    )
  }

  PA <- clusters_details |>
    purrr::pmap_dfr(\(cluster, cluster_size, sample_size, ...) {
      ES_joined <- ES_joined |>
        dplyr::filter(.data$cluster == cluster) |>
        dplyr::select(-dplyr::all_of(c("cluster"))) |>
        dplyr::sample_n(sample_size, replace = cluster_size < sample_size)

    })


  checkmate::assert_data_frame(PA,
                               any.missing = FALSE,
                               nrows = nrow(P),
                               null.ok = FALSE)
  PPA <- P |>
    dplyr::relocate(dplyr::all_of(c(ES_idx, sp_name))) |>
    dplyr::bind_rows(
      PA |>
        dplyr::select(dplyr::all_of(names(P)))
    )

  return(PPA)
  }
