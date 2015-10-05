#' Execute a function and cache the result as a dplyr database backed tbl.
#'
#' @param src dplyr database source object. This should be a database.
#' @param name base name of the table to create.
#' @param creator function that, when executed, returns a data frame or tbl result to be cached.
#' @param ... options to be passed to the \code{creator} function.
#' @param tag string appended to base name to form database table name. If \code{volatile==TRUE}
#' then \code{tag} is set to \code{as.integer( Sys.time() )} automatically.
#' @param volatile if \code{TRUE} then a temporary table is created.
#' @inheritParams dplyr:::copy_to.src_sql
#' @param geoindexes a list of character vectors. Each element of the list will create a new GIST index.
#' @seealso \code{\link{dplyr::copy_to.src_sql}}
#' @description Execute the long running function \code{creator()} and cache the value in \code{src}.
#' Subsequent calls to \code{tbl_cache} with the same \code{name} and \code{tag} will return a reference to the cached
#' table in \code{src}. The cached table can be indexed using either standard indexes or
#' geospatial indexes. If preferred a temporary table can be created.
#' @return A dplyr database backed tbl.
#' @export
tbl_cache = function(src, name, creator, ..., tag = strftime( Sys.time(), format="%Y%m%d"), volatile=FALSE, types = NULL, temporary = FALSE, indexes = NULL, analyze = TRUE, geoindexes=NULL ) {
  if (volatile) {
    tag = as.integer( Sys.time() )
    temporary = TRUE
  }

  name = tbl_name(name, tag)
  if (!db_has_table(src$con, name) ) {
    result = creator(...)

    if ( "tbl_sql" %in% class(result) ) {
      TBL = compute(result, name, temporary=FALSE)
      dplyr:::db_create_indexes(src$con, as.character(TBL$from), indexes = indexes)
    } else {
      TBL = copy_to(src, result, name=name, types=types, temporary=temporary, indexes=indexes, analyze=analyze)
    }

    if (!is.null(geoindexes)) {
      db_create_geo_indexes( src$con, name, geoindexes )
    }
  } else {
    TBL = tbl(src, name)
  }

  return(TBL)
}
