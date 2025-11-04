#' Import structural metadata for the BPM6 table
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. It uses the functions from the UMARimportR package to
#' insert the data into the database.
#'
#' @param con connection to database
#' @param schema schema name, defaults to "platform"
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
BPM6_import_structure <- function(con,
                                   schema = "platform",
                                   keep_vintage = FALSE) {
  message("Importing structure data for BPM6 into schema ", schema)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(con, schema, keep_vintage)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert: ", sum(insert_results$table$count), " rows")
   # prepare and insert category table table
  category_table_table <- prepare_category_table_table(con,schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(con,  schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table <- prepare_dimension_levels_table(con,  schema)
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(con,  schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}
