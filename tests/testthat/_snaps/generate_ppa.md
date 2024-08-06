# generate_ppa.data.frame - P is a vector and ES_idx not informed.

    Code
      ppa <- generate_ppa(dplyr::bind_cols(dplyr::select(ES, -cell_id), P), c("sp1",
        "sp2", "sp3"), ppa_function = my_func)
    Condition
      Warning:
      ! ES_idx is not informed.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# generate_ppa.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is T.

    Code
      ppa <- generate_ppa(dplyr::bind_cols(ES, P), c("sp1", "sp2", "sp3"), ES_idx = "cell_id",
      ppa_function = my_func)
    Condition
      Warning:
      ! cell_id is present in ES but is not present in P.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# generate_ppa.data.frame - P is a vector and ES_idx is informed and remove_Es_idx is F.

    Code
      ppa <- generate_ppa(dplyr::bind_cols(ES, P), c("sp1", "sp2", "sp3"), ES_idx = "cell_id",
      ppa_function = my_func, remove_ES_idx = FALSE)
    Condition
      Warning:
      ! cell_id is present in ES but is not present in P.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# generate_ppa.data.frame - P is a DF and ES_idx not informed.

    Code
      ppa <- generate_ppa(dplyr::select(ES, -cell_id), P, ppa_function = my_func)
    Condition
      Warning:
      ! ES_idx is not informed.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# generate_ppa.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is T.

    Code
      ppa <- generate_ppa(ES, P, ES_idx = "cell_id", ppa_function = my_func)
    Condition
      Warning:
      ! cell_id is present in ES but is not present in P.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# generate_ppa.data.frame - P is a DF and ES_idx is informed and remove_Es_idx is F.

    Code
      ppa <- generate_ppa(ES, P, ES_idx = "cell_id", ppa_function = my_func,
        remove_ES_idx = FALSE)
    Condition
      Warning:
      ! cell_id is present in ES but is not present in P.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.

# ppa_random.data.frame - Throws different errors to 3 species.

    Code
      ppa <- generate_ppa(dplyr::select(dplyr::filter(dplyr::bind_cols(ES, P), sp1 >
      0), -c(sp1, sp2, sp3)), dplyr::filter(P, sp1 > 0), ES_idx = "cell_id",
      remove_ES_idx = FALSE, ppa_function = my_func)
    Condition
      Warning:
      ! cell_id is present in ES but is not present in P.
      i ES and P have the same number of rows.
      i ES and P were bonded using their row number.
      Warning:
      ! Problems occurs to process some species.
      i (sp1) : Assertion on nrow(PA) failed. Element 1 is not >= 88.
      i (sp2) : Assertion on PPA failed. Contains only missing values.
      i (sp3) : Assertion on PPA failed. Contains only missing values.

