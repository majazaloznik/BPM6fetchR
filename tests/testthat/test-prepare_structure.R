test_that("prepare table table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    table_table <- prepare_table_table(con_test)
    expect_equal(nrow(table_table), 1)
    expect_equal(ncol(table_table), 6)
    expect_true(all(names(table_table) %in%
                      c("name", "notes", "source_id", "url", "code", "keep_vintage")))
  })
})

test_that("prepare category table table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_category_table_table(con_test)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c( "table_id","category_id", "source_id"))
    expect_equal(out$source_id[1], 5)
    expect_equal(out$category_id[1], 26)
  })
})

test_that("prepare table dinemsions table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_table_dimensions_table(con_test)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("table_id", "dimension", "is_time"))
    expect_equal(out$dimension, "Klasifikacija")
    expect_equal(out$is_time, FALSE )
  })
})

test_that("prepare dimensions levels table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_dimension_levels_table(con_test)
    expect_equal(nrow(out), 1531)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c( "level_value", "level_text", "tab_dim_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare series table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- series_table <- prepare_series_table(con_test)
    expect_equal(nrow(out), 1531)
    expect_equal(ncol(out), 5)
    expect_equal(names(out), c("table_id", "name_long", "code", "unit_id", "interval_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare series levels table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_series_levels_table(con_test)
    expect_equal(nrow(out),1531)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("series_id",  "level_value", "tab_dim_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})
