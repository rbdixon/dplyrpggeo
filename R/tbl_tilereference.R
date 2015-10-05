#' Map geographic point to slippy tile indices.
#'
#' @description This function converts a geographic point object (\code{ST_Point(lon, lat)}) to
#'Slippy tile indices (xindex, yindex, level). This function alters the
#' table referenced by \code{DATA} to add new tile indices columns.
#' @param DATA database backed dplyr tbl.
#' @param loc name of column containing a geographic point object to be mapped to a tile.
#' @param cols names of columns to be created to hold x, y, and level tile indices.
#' @param level numeric tile level to map to.
#' @return dplyr tbl object
#' @note This function alters the database schema permanantly. If the schema has already been
#' modified to include the columns referenced by \code{cols} then the table is returned
#' unmodifed with a warning. This function also requires custom PostGIS functions from the
#' \href{https://github.com/petewarden/postgis2gmap/blob/master/README.md}{postgis2gmap}
#' project. These functions must be loaded into PostgreSQL in advance.
#' @export

tbl_tilereference = function(DATA, loc="loc", cols=c("xindex", "yindex", "level"), level=18) {
  if (!("tbl_sql" %in% class(DATA))) stop("Table must be in database")

  name = as.character(DATA$from)
  src = DATA$src
  con = src$con

  if ( any( c(cols, level) %in% colnames(DATA) ) ) {
    warning( sprintf("Table %s already has a tile-referenced column %s, %s, or %s. Returning unaltered table.", name, cols[1], cols[2], level))
    return(DATA)
  }

  sql = sprintf('
                ALTER TABLE %s
                  ADD COLUMN %s INTEGER,
                  ADD COLUMN %s INTEGER,
                  ADD COLUMN %s INTEGER;

                CREATE INDEX %s_tile_ndx ON %s (%s, %s, %s);

                UPDATE %s SET
                %s = FLOOR(ST_X(tile_indices_for_lonlat( %s, %s))),
	              %s = FLOOR(ST_Y(tile_indices_for_lonlat( %s, %s))),
	              %s = %s
                ',
                # ALTER
                name,
                cols[1],
                cols[2],
                cols[3],
                # INDEX
                name,
                name,
                cols[1],
                cols[2],
                cols[3],
                # UPDATE xindex
                name,
                cols[1],
                loc,
                level,
                # UPDATE yindex
                cols[2],
                loc,
                level,
                # UPDATE level
                cols[3],
                level
  )

  # Alter the table and georeference
  db_begin(con)
  on.exit( db_rollback(con) )
  RPostgreSQL::dbSendQuery(con, sql)
  # Required since we have new columns
  result = tbl(src, name)
  db_commit(con)

  # Commit and exit
  on.exit(NULL)
  return(result)
}
