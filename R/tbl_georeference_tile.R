#' Map named slippy tile indices to a geographic point and polygon representation.
#'
#' @description Slippy tile indices can not be displayed in GIS tools without mapping their
#' indices (xindex, yindex, level) to a geographic representation. This function alters the
#' table referenced by \code{DATA} to add two new geography columns describing the upper left
#' corner of the tile and the boundary of the tile.
#' @param DATA database backed dplyr tbl.
#' @param cols names of columns containing x, y, and level tile indices.
#' @param loc name of new column to be created with geographic point object representing the upper
#' left hand corner of the tile.
#' @param boundary name of new column to be created with the geographic polygon object representing
#' the tile bounday.
#' @return dplyr tbl object
#' @note This function alters the database schema permanantly. If the schema has already been
#' modified to include the \code{loc} or \code{boundary} column then the table is returned
#' unmodifed with a warning. This function also requires custom PostGIS functions from the
#' \href{https://github.com/petewarden/postgis2gmap/blob/master/README.md}{postgis2gmap}
#' project. These functions must be loaded into PostgreSQL in advance.
#' @export

tbl_georeference_tile = function( DATA, cols=c("xindex", "yindex", "level"), loc="tileloc", boundary="tilebounds" ) {
  if (!("tbl_sql" %in% class(DATA))) stop("Table must be in database")

  name = as.character(DATA$from)
  src = DATA$src
  con = src$con

  if ( any( c(loc, boundary) %in% colnames(DATA) ) ) {
    warning( sprintf("Table %s already has a georeferenced column %s or %s. Returning unaltered table.", name, loc, boundary))
    return(DATA)
  }

  sql = sprintf('
                ALTER TABLE %s
                ADD COLUMN %s geography;

                ALTER TABLE %s
                ADD COLUMN %s geography;

                CREATE INDEX %s_%s_gix ON %s USING GIST (%s);
                CREATE INDEX %s_%s_gix ON %s USING GIST (%s);

                UPDATE %s
                SET
                  %s = lonlat_for_tile_indices(%s, %s, %s)::geography,
                  %s = bounds_for_tile_indices(%s, %s, %s);
                ',
                # ALTER loc
                name,
                loc,
                # ALTER boundary
                name,
                boundary,
                # INDEX loc
                name,
                loc,
                name,
                loc,
                # INDEX boundary
                name,
                boundary,
                name,
                boundary,
                # UPDATE loc
                name,
                loc,
                cols[2],
                cols[1],
                cols[3],
                # UPDATE boundary
                boundary,
                cols[2],
                cols[1],
                cols[3]
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
