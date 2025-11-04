parsed <- extract_all_bpm6_sheets("tests/testthat/testdata/6PB(brez memo)_08_2025.xlsx")

bpm6_levels <- parsed |>
  dplyr::select(code, description) |>
  unique()

usethis::use_data(bpm6_levels,
                  internal = FALSE,
                  overwrite = TRUE)
