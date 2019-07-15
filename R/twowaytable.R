basic <- function(df, exposure, outcome) {
  exposure <- enquo(exposure)
  outcome <- enquo(outcome)

  outcome_levels <- df %>%
    select(!!outcome) %>%
    unique() %>%
    unlist() %>% as.vector() %>% # Convert from data frame to vector
    as.character() # Necessary in case the exposure levels are initially numeric

  df %>%
    group_by(!!exposure, !!outcome) %>%
    tally() %>%
    spread(!!outcome, n) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    rename_at( # Add label to column names (outcome=...)
      vars(outcome_levels),
      function(x) paste0(rlang::quo_text(outcome), "=", x)
    ) %>%
    mutate_at( # Add label to first column (exposure=...)
      vars(!!exposure), function(x) str_c(rlang::quo_text(exposure), "=", x)
    ) %>%
    rename_at(vars(!!exposure), function(x) "↓ Exposure / Outcome →")
}

with_col_percentages <- function(df, exposure, outcome) {
  df <- twowaytable::basic(df, !!enquo(exposure), !!enquo(outcome))
  df %>%
    mutate_at(
      seq(2, ncol(df)),
      function(x) {
        str_c(x, " (", round(x / sum(x) * 100), "%)")
      }
    )
}

with_row_percentages <- function(df, exposure, outcome) {
  twowaytable::with_col_percentages(df, !!enquo(outcome), !!enquo(exposure)) %>% t
}
