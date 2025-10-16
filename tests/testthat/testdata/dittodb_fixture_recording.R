# This code was run once and is here for archival purposes.
# devtools::install_github("majazaloznik/UMARaccessR")
# devtools::install_github("majazaloznik/UMARimportR")
source("tests/testthat/helper-connection.R")



xx <- BPM6fetchR:::extract_bpm6_sheet("tests/testthat/testdata/6PB(brez memo)_08_2025.xlsx", "BPM6 vse")
zz <- BPM6fetchR:::extract_bpm6_sheet("tests/testthat/testdata/6PB - vse - uradne (EUR) (za SURS) - revizija 2020-2024.xlsx", "2024")

xxx <- extract_all_bpm6_sheets("tests/testthat/testdata/6PB(brez memo)_08_2025.xlsx")
zzz <- extract_all_bpm6_sheets("tests/testthat/testdata/6PB - vse - uradne (EUR) (za SURS) - revizija 2020-2024.xlsx")
