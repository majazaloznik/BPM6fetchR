
#' Prepare table to insert into `table` table
#'
#' Helper function that prepares the table metadata from desezoniranje_config.
#' Returns a list of tables ready to insert into the `table` table with the
#' db_writing family of functions from `UMARimportR`.
#'
#' @param con Connection to the database
#' @param schema Schema name, defaults to "platform"
#' @param keep_vintage Logical indicating whether to keep vintages, defaults to FALSE
#'
#' @return A list of dataframes, one for each unique table_id in the config.
#'   Each dataframe contains `code`, `name`, `source_id`, `url`, and `notes` columns.
#'
#' @export
prepare_table_table <- function(con,
                                schema = "platform",
                                keep_vintage = FALSE) {
  data.frame(code = "BPM6",
             name = "Pla\u010dilna bilanca po BPM6",
             source_id = UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema),
             url = NA,
             notes = jsonlite::toJSON(list(), auto_unbox = TRUE),
             keep_vintage = keep_vintage)
  }



#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category_table` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
prepare_category_table_table <- function(con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  category_id <- UMARaccessR::sql_get_category_id_from_name("Pla\u010dilna bilanca", con, source_id, schema)
  table_id <- UMARaccessR::sql_get_table_id_from_table_code(con, "BPM6", schema)

 data.frame(table_id, category_id, source_id)
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that manually prepares the table_dimensions table.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `table_id`, `dimension`, `is_time` columns for
#' each dimension of this table.
#' @export
prepare_table_dimensions_table <- function(con, schema = "platform"){
  data.frame(table_id = UMARaccessR::sql_get_table_id_from_table_code(con, "BPM6", schema),
             dimension = "Klasifikacija",
             is_time = FALSE)
}


#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that manually prepares the dimension_levels for each
#' table and gets their codes and text from desezoniranje_config.
#' Returns a single dataframe ready to insert into the `dimension_levels` table
#' with the db_writing family of functions.
#'
#' @param con Connection to the database
#' @param schema Schema name, defaults to "platform"
#'
#' @return A dataframe with `tab_dim_id`, `level_value`, and `level_text` columns
#'   for all tables and both dimensions (Meritev and Seasonally adjusted).
#' @export
prepare_dimension_levels_table <- function(con,
                                           schema = "platform") {

  table_id <- UMARaccessR::sql_get_table_id_from_table_code(con, "BPM6", schema)

  tab_dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(
    table_id, "Klasifikacija", con, schema)

  bpm6_levels |>
    dplyr::rename(level_value = code,
           level_text = description) |>
    dplyr::mutate(tab_dim_id = tab_dim_id)

}


#' Prepare table to insert into `series` table
#'
#' Prepares series metadata for all tables in desezoniranje_config.
#'
#' @param con Connection to the database
#' @param schema Schema name, defaults to "platform"
#'
#' @return A dataframe with columns: `table_id`, `name_long`, `code`,
#'   `unit_id`, and `interval_id` for each series.
#' @export
prepare_series_table <- function(con,
                                 schema = "platform") {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, "BPM6", schema)

  bpm6_levels |>
    dplyr::rename(name_long = description) |>
    dplyr:: mutate(table_id = tbl_id,
           code = paste0("BS--BPM6--", code, "--M"),
           unit_id = UMARaccessR::sql_get_unit_id_from_unit_name("mio eur", con, schema),
           interval_id = "M") |>
    dplyr::select(table_id, name_long, code, unit_id, interval_id)
}


#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions. Of course after the series have been inserted.
#'
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `series_id`, `tab_dim_id`, `level_value` columns
#' all the series-level combinations for this table.
#' @export
#'
prepare_series_levels_table <- function(con,
                                        schema = "platform") {

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, "BPM6", schema)

    # Get  dimensions code
    dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
      dplyr::pull(id)

    # Parse series codes - now has 5 parts: source--table--meritev--sa--interval
    UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
      dplyr::select(id, code) |>
      tidyr::separate(code, into = c("source", "table", "klasifikacija", "interval"), sep = "--") |>
      dplyr::select(series_id = id, level_value = klasifikacija) |>
      dplyr::mutate(tab_dim_id = dimz) |>
      as.data.frame()
}
