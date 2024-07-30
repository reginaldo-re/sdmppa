#' @title Calculate Presences and Pseudo-absences Using Clusterizationfrom an
#' Enviromental Space ES and presences P
#'
#' @description
#' `ppa_cluster` calculates a set of pseudo-absences points (PAP) **randomly**
#' drawn from the enviromental space denoted by ES.
#'
#' @param ES A set representing the environmental space.
#'
#' @param PP A set representing de presence points.
#' @param num_clusters Clsuters
#'
#' @details
#'
#' In fact, the data type of parameter **ES** is not a `set`, but a `dataframe`.
#' Similarly as **ES**, **PP** is not really a `set`, It must be a `dataframe`
#' or a `list`. Points in **PP** represent presence points that wil be removed
#' from **ES**, once it is necessary to reduce **ES** so that **PAP** and **PP**
#' keep disjoint. Therefore, **PP** can be:
#'  1. A `dataframe`, such that at least a name of one column in **ES**  must
#'  match **PP**.
#'  2. A named list, such that the name of the list must match a column name in
#'  **ES**, as an index. So,
#'  the values in the named list must reference rows in **ES** that can be
#'  joined using the name of the list.
#'  3. A list containing both dataframes (as in 1) and named lists (as in 2).
#'
#' How is the PPA computed?
#'
#' When a `dataframe` is passed to **PP**, all rows in **ES** whose column names
#' match *PP* are compared and removed if they are equal. This reduces the
#' environmental space in **ES** to such that It does not contain *PP*, and
#' avoids common points in **PP** and **PAP**. There are two choices when a list
#' is passed: the list is an index referencing rows in **ES**, and the name of
#' the list must match a column in **ES**; or, it is an unnamed list containing
#' data frames and other inner named lists representing several sets **PP**. In
#' both cases, a `dataframe` or a named list, **PP** should match at least one
#' column in **ES**.
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
#'   )
#' )
#' PP1 <- dplyr::tibble(
#'   cell_id = c(75000, 81017, 84360, 86468),
#'   bio_1 = c(25.24835, 26.47237, 25.43329, 25.84756),
#'   bio_4 = c(48.37795, 54.44825, 41.39428, 46.00434)
#' )
#'
#' PAP <- P |> ppa_cluster(PP1)
#' PAP <- P |> ppa_cluster(list("cell_id" = PP1$cell_id))
#' PAP <- P |> ppa_cluster(list(PP1, PP1, PP1))
#' PAP <- P |> ppa_cluster(
#'   list(
#'     list("cell_id" = PP1$cell_id),
#'     list("cell_id" = PP1$cell_id),
#'     list("cell_id" = PP1$cell_id)
#'   )
#' )
#'
#' @export ppa_cluster
#' @rdname ppa_cluster
ppa_cluster <- function(ES, PP, ES_idx, num_clusters, perc_pres) {
  UseMethod("ppa_cluster")
}


#' @return \code{NULL}
#'
#' @rdname ppa_cluster
#' @method ppa_cluster data.frame
#' @export
ppa_cluster.data.frame <- function(ES, PP = NULL, ES_idx = NULL,
                                   num_clusters = 0, perc_pres = 10) {
    return(
      generate_ppa(
        ES = ES,
        P = P,
        ES_idx = ES_idx,
        ppa_function = .ppa_cluster,
        num_clusters,
        perc_pres
        )
      )
  }


.ppa_cluster <- function(ES = NULL, PP = NULL, ES_idx = NULL, num_clusters = 0,
                         perc_pres = 10) {
    num_clusters <- ifelse(num_clusters == 0,
                           max(round(nrow(PP) * perc_pres / 100), 1),
                           num_clusters)

    names_ES <- names(ES)
    names_PP <- names(PP)
    names_both <- names_PP[names_PP %in% names_ES]


    PPA <- NULL
    set.seed(nrow(P))

    ES_joined <- ES |>
      dplyr::left_join(PP, by = names_both) |>
      dplyr::mutate(presence = tidyr::replace_na(.data$presence, 0))

    if (ES_joined$presence |> purrr::discard(is.na) |> sum() != nrow(PP)) {
      points_not_matched <- ES_joined$presence |>
        purrr::discard(is.na) |>
        sum()
      cli::cli_abort(
        c("x" = "Something goes wrong joining ES to PP!",
          "i" = "{points_not_matched} points in PP not matches P using variables that compose the enviromental space.")
      )
    }

    set.seed(nrow(PP))
    clusters <- stats::kmeans(ES, centers = num_clusters)
    ES_joined$cluster <- clusters$cluster

    clusters_details <- ES_joined |>
      dplyr::group_by(.data$cluster) |>
      dplyr::summarise(
        cluster_size = dplyr::n(),
        sample_size = (dplyr::n() / nrow(ES_joined) * nrow(PP)) |> round()
      )

    diff_calc <- nrow(PP) - (clusters_details$sample_size |> sum())
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

    PAP <- clusters_details |>
      purrr::pmap_dfr(\(cluster, cluster_size, sample_size, ...) {
        ES_joined |>
          dplyr::filter(.data$cluster == cluster &
                          .data$presence == 0) |>
          dplyr::select(-c("cluster", "presence")) |>
          dplyr::sample_n(sample_size, replace = cluster_size < sample_size)
      })

    checkmate::assert_data_frame(PAP,
                                 any.missing = FALSE,
                                 nrows = nrow(PP),
                                 null.ok = FALSE)
    return(PAP)
  }
