#' Map named latitude and longitude columns to a geographic point object.
#'
#' @description Latitude and longitude coordinates must be mapped to a PostGIS object before
#' they can be manipulated by PostGIS functions or displayed in GIS tools. This function alters
#' the table referenced by \code{DATA} to add a new geography column named \code{loc} and
#' populates the column with the value of \code{ST_Point(lon, lat)}.
#' @param DATA database backed dplyr tbl.
#' @param cols names of columns containing longitude and latitude as decimal degrees.
#' @param loc name of new column to be created with geographic point object.
#' @return dplyr tbl object
#' @note This function alters the database schema permanantly. If the schema has already been
#' modified to include the \code{loc} column then the table is returned unmodifed with a
#' warning.
#' @seealso \href{http://postgis.net/docs/ST_Point.html}{ST_Point()}
#' @export

tbl_georeference_latlon = function(DATA, cols=c("lon", "lat"), loc="loc") {
  if (!("tbl_sql" %in% class(DATA))) stop("Table must be in database")

  name = as.character(DATA$from)
  src = DATA$src
  con = src$con

  if ( loc %in% colnames(DATA) ) {
    warning( sprintf("Table %s already has a georeferenced column %s. Returning unaltered table.", name, loc))
    return(DATA)
  }

  sql = sprintf('
    ALTER TABLE %s
    ADD COLUMN %s geography;

    CREATE INDEX %s_%s_gix ON %s USING GIST (%s);

    UPDATE %s
    SET %s = ST_POINT(
      cast(%s AS double precision),
      cast(%s AS double precision)
    )::geography
    ',
                # ALTER
                name,
                loc,
                # INDEX
                name,
                loc,
                name,
                loc,
                # UPDATE
                name,
                loc,
                cols[1],
                cols[2]
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
