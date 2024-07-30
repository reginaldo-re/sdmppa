# .generate_single_ppa.data.frame - Problems in joinning ES and P using ES_idx.

    Code
      .generate_single_ppa(ES, dplyr::mutate(dplyr::bind_cols(dplyr::select(P, sp1),
      ES), cell_id = 1), ES_idx = "cell_id", ppa_function = function(ES, P, ES_idx,
        sp_name) { }, sp_name = "sp1")
    Condition
      Error in `c()`:
      ! argument 3 is empty

# generate_ppa.data.frame - P has two columns matching ES, and ES_idx is defined and matches P and ES.

    Code
      generate_ppa(ES, dplyr::bind_cols(P, dplyr::select(ES, cell_id, bio_1)),
      ppa_function = function(ES, P, ES_idx, sp_name) {
        if (sp_name == "sp1") {
          return(PPA_sp1)
        } else if (sp_name == "sp2") {
          return(PPA_sp2)
        } else {
          return(PPA_sp3)
        }
      }, ES_idx = "cell_id")
    Condition
      Error in `generate_ppa()`:
      x There exists more than one column matching P and ES.
      i 2 columns: cell_id and bio_1.

# generate_ppa.data.frame - P has one element matching ES, and ES_idx is defined and matches ES.

    Code
      generate_ppa(dplyr::select(dplyr::bind_cols(ES, P), -dplyr::all_of(c("sp1",
        "sp2", "sp3"))), c("cell_id"), ppa_function = function(ES, P, ES_idx, sp_name)
        {
          if (sp_name == "sp1") {
            return(dplyr::mutate(PPA_sp1, .ES_idx = 1))
          } else if (sp_name == "sp2") {
            return(dplyr::mutate(PPA_sp2, .ES_idx = 1))
          } else {
            return(dplyr::mutate(PPA_sp3, .ES_idx = 1))
          }
        }, ES_idx = "cell_id")
    Condition
      Warning:
      x There are not variables representing enviromental space in P.
      i Please check the P and ES.
    Output
      named list()

# generate_ppa.data.frame - Problems in joinning ES and P using ES_idx.

    Code
      generate_ppa(ES, dplyr::mutate(dplyr::select(P, sp1), cell_id = 1), ES_idx = "cell_id",
      ppa_function = function(ES, P, ES_idx, sp_name) { }, sp_name = "sp1")
    Condition
      Error in `generate_ppa()`:
      x 40222 rows of P can't be joined to ES by cell_id.
      i Please check the P.

