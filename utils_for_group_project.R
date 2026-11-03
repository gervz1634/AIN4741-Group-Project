train_test_split <- function(df, proportion = 0.7) {
  set.seed(123)
  df <- df %>%
    mutate(across(all_of(cat_col), as.factor))

  train_idx <- c(1:ceiling(nrow(df) * proportion))
  df_train <- df[train_idx, ]
  df_test <- df[-train_idx, ]

  return(
    list(
      full = df,
      train = df_train,
      test = df_test
    )
  )
}


cat_EDA <- function(df, cat_col) {
  for (v1 in cat_col) {
    for (v2 in cat_col) {
      if (v1 < v2) {
        cat("\n=====", v1, "vs", v2, "=====\n")
        print(table(df[[v1]], df[[v2]]))
      }
    }
  }
}


theme_ev <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(colour = "grey35", size = base_size - 1),
      plot.caption = element_text(colour = "grey45", size = base_size - 3, hjust = 0),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}
