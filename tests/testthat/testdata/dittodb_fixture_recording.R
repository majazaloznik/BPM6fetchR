# This code was run once and is here for archival purposes.
# devtools::install_github("majazaloznik/UMARaccessR")
# devtools::install_github("majazaloznik/UMARimportR")
source("tests/testthat/helper_connection.R")

con <- make_connection()


start_db_capturing()
con_test <- make_test_connection()
table_table <- prepare_table_table(con_test)
stop_db_capturing()


start_db_capturing()
con_test <- make_test_connection()
out <- prepare_category_table_table(con_test)
stop_db_capturing()


start_db_capturing()
con_test <- make_test_connection()
out <- prepare_table_dimensions_table(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
out <- prepare_dimension_levels_table(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
out <- prepare_dimension_levels_table(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
out <-  prepare_series_table(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
out <- prepare_series_levels_table(con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
results <- BPM6_import_structure(con_test)
stop_db_capturing()


# xx <- BPM6fetchR:::extract_bpm6_sheet("tests/testthat/testdata/6PB(brez memo)_08_2025.xlsx", "BPM6 vse")
# zz <- BPM6fetchR:::extract_bpm6_sheet("tests/testthat/testdata/6PB - vse - uradne (EUR) (za SURS) - revizija 2020-2024.xlsx", "2024")
#
# xxx <- extract_all_bpm6_sheets("tests/testthat/testdata/6PB(brez memo)_08_2025.xlsx")
# zzz <- extract_all_bpm6_sheets("tests/testthat/testdata/6PB - vse - uradne (EUR) (za SURS) - revizija 2020-2024.xlsx")
