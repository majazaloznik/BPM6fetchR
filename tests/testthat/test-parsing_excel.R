# tests/testthat/test-parsing_excel.R

test_that("format_period_from_date converts Excel dates correctly", {
  # Test various Excel date serials
  expect_equal(format_period_from_date(45688), "2025M01")  # 31/01/2025
  expect_equal(format_period_from_date(45716), "2025M02")  # 28/02/2025
  expect_equal(format_period_from_date(45747), "2025M03")  # 31/03/2025
  expect_equal(format_period_from_date(44957), "2023M01")  # 31/01/2023 (corrected)
  expect_equal(format_period_from_date(45322), "2024M01")  # 31/01/2024
})

test_that("extract_bpm6_sheet reads partial year sheet correctly", {
  test_file <- testthat::test_path("testdata", "test_bpm6_partial.xlsx")
  result <- extract_bpm6_sheet(test_file, "BPM6 vse")

  # Check structure
  expect_equal(names(result), c("code", "description", "period_id", "value"))

  # Check we have 5 codes × 8 months = 40 rows
  expect_equal(nrow(result), 40)

  # Check codes are parsed correctly
  expect_true("0" %in% result$code)
  expect_true("1" %in% result$code)
  expect_true("1.1" %in% result$code)
  expect_true("1.1.1" %in% result$code)
  expect_true("1.1.1.1" %in% result$code)

  # Check descriptions are parsed correctly
  desc_0 <- result$description[result$code == "0"][1]
  expect_equal(desc_0, "Plačilna bilanca (BPM6)")

  desc_111 <- result$description[result$code == "1.1.1"][1]
  expect_equal(desc_111, "Izvoz blaga")

  # Check period_ids are formatted correctly
  expect_true(all(stringr::str_detect(result$period_id, "^\\d{4}M\\d{2}$")))
  expect_true("2025M01" %in% result$period_id)
  expect_true("2025M08" %in% result$period_id)
  expect_false("2025M09" %in% result$period_id)  # Should not have Sept

  # Check values are numeric
  expect_true(all(is.numeric(result$value)))

  # Check a specific value
  # Code "0", first month (2025M01) should be 100
  val_0_m1 <- result$value[result$code == "0" & result$period_id == "2025M01"]
  expect_equal(val_0_m1, 100)
})

test_that("extract_bpm6_sheet reads full year sheet correctly", {
  test_file <- testthat::test_path("testdata", "test_bpm6_multiyear.xlsx")
  result <- extract_bpm6_sheet(test_file, "2024")

  # Check structure
  expect_equal(names(result), c("code", "description", "period_id", "value"))

  # Check we have 5 codes × 12 months = 60 rows
  expect_equal(nrow(result), 60)

  # Check all 12 months are present for 2024
  expect_true(all(paste0("2024M", sprintf("%02d", 1:12)) %in% result$period_id))

  # Check quarterly data is NOT included
  expect_false(any(stringr::str_detect(result$period_id, "Q")))

  # Check a specific value
  # Code "1.1", December 2024 (month 12) should be 200 * 12 + 2024
  val_11_m12 <- result$value[result$code == "1.1" & result$period_id == "2024M12"]
  expect_equal(val_11_m12, 200 * 12 + 2024)
})

test_that("extract_bpm6_sheet handles malformed data gracefully", {
  # This test would need a malformed test file
  # Skip for now if file doesn't exist
  skip("Requires malformed test file")
})

test_that("extract_all_bpm6_sheets combines multiple sheets correctly", {
  test_file <- testthat::test_path("testdata", "test_bpm6_multiyear.xlsx")

  result <- extract_all_bpm6_sheets(test_file)

  # Should have 5 codes × 24 months (12 months × 2 years) = 120 rows
  expect_equal(nrow(result), 120)

  # Should have data from both 2023 and 2024
  expect_true(any(stringr::str_detect(result$period_id, "^2023M")))
  expect_true(any(stringr::str_detect(result$period_id, "^2024M")))

  # All 5 codes should be present
  unique_codes <- unique(result$code)
  expect_equal(length(unique_codes), 5)
  expect_true(all(c("0", "1", "1.1", "1.1.1", "1.1.1.1") %in% unique_codes))

  # Each code should have 24 periods (12 months × 2 years)
  code_counts <- table(result$code)
  expect_true(all(code_counts == 24))
})

test_that("extract_all_bpm6_sheets with explicit sheet names", {
  test_file <- testthat::test_path("testdata", "test_bpm6_multiyear.xlsx")

  # Only process 2024 sheet
  result <- extract_all_bpm6_sheets(test_file, sheet_names = "2024")

  # Should have 5 codes × 12 months = 60 rows
  expect_equal(nrow(result), 60)

  # Should only have 2024 data
  expect_true(all(stringr::str_detect(result$period_id, "^2024M")))
  expect_false(any(stringr::str_detect(result$period_id, "^2023M")))
})

test_that("extract_all_bpm6_sheets errors on duplicate periods", {
  test_file <- testthat::test_path("testdata", "test_bpm6_duplicate.xlsx")

  # Should error when sheets have overlapping data
  expect_error(
    extract_all_bpm6_sheets(test_file),
    "duplicate period_ids"
  )
})

test_that("extract_all_bpm6_sheets handles single sheet file", {
  test_file <- testthat::test_path("testdata", "test_bpm6_partial.xlsx")

  result <- extract_all_bpm6_sheets(test_file)

  # Should have 5 codes × 8 months = 40 rows
  expect_equal(nrow(result), 40)

  # Should have 2025 data only
  expect_true(all(stringr::str_detect(result$period_id, "^2025M")))

  # Check specific months are present
  unique_periods <- unique(result$period_id)
  expected_periods <- paste0("2025M", sprintf("%02d", 1:8))
  expect_equal(sort(unique_periods), sort(expected_periods))
})
