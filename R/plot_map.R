#' Check field map layout for errors and duplicate range/row locations.
#'
#' This function can map either a single trial or all trials. When mapping all trials,
#' best practice is to first filter out trials without range/row. If you do this and
#' still receive a warning that the output will contain list cols, you have duplicates
#' in range/row.
#'
#' @param df A data frame.
#' @param id_col A string. Column containing a unique identifier for the trial. The default is 'trial'.
#' @param trial_id A string. Use "all" to map all data by trial; for single trial, trial id string
#' to locate in id_col, i.e. "OregonCereals_SWS_ION_2023". The string to match should be exact; the function
#' is not currently set up to locate partial strings.
#' @param plot_id A string or vector; column name(s) to use as plot identifier. Currently can take up to
#' two inputs, i.e. c("plot", "ibloc"). This is useful for finding missing plot numbers, ibloc, entry, variety, etc.
#' @param range Column containing range values; the default is 'range'
#' @param row Column containing row locations; the default is 'row'
#' @family clean column contents functions
#' @export


plot_map <- function(df, id_col = "trial", trial_id, plot_id = NULL, range = range, row = row) {

  trial_id <- rlang::as_string(trial_id)
  id_col <- rlang::sym(id_col)

  if (trial_id == "all") {
    df <- df %>% group_by(!!id_col)

  } else {
  df <- df %>% filter(!!id_col == trial_id)
  }

  if (length(plot_id) > 1) {

    plot_id1 <- rlang::sym(plot_id[1])
    plot_id2 <- rlang::sym(plot_id[2])

    df2 <- df %>%
      mutate(across(all_of(plot_id), as.character)) %>%
      mutate(new_plot_id = paste(!!plot_id1, "\t", !!plot_id2)) %>%
      select(!!id_col, new_plot_id, range, row) %>%
      tidyr::pivot_wider(names_from = row, values_from = new_plot_id,
                         names_prefix = "row", names_sort = TRUE) %>%
      arrange(!!id_col, desc(range))

  } else {

      df2 <- df %>%
        select(!!id_col, {{ plot_id }}, range, row) %>%
        tidyr::pivot_wider(names_from = row, values_from = {{ plot_id }},
                           names_prefix = "row", names_sort = TRUE) %>%
        arrange(!!id_col, desc(range))
  }

  df2
}
