db_create_geo_index = function (con, table, columns, name = NULL, ...)
{
  UseMethod("db_create_geo_index")
}

# Cribbed from dplyr
`%||%` = function (x, y) {
  if (is.null(x)) y else x
}

db_create_geo_index.DBIConnection = function(con, table, columns, name = NULL, ...)  {
  name = name %||% paste0(c(table, columns, "ndx"), collapse = "_")
  fields = escape(ident(columns), parens = TRUE, con = con)

  sql <- build_sql("CREATE INDEX ", ident(name), " ON ", ident(table),
                   " USING GIST ", fields, con = con)
  RPostgreSQL::dbGetQuery(con, sql)
}

db_create_geo_indexes = function (con, table, indexes = NULL, ...) {
  if (is.null(indexes))
    return()
  assert_that(is.list(indexes))
  for (index in indexes) {
    db_create_geo_index(con, table, index, ...)
  }
}
