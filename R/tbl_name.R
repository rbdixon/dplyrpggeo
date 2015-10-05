#' Create a temporary table name.
#'
#' @param name base name for table
#' @param tag string to append to table base name
#' @return character vector containing a table name.
#' @export
tbl_name = function(name="temp", tag=as.integer(Sys.time()) ) {
  sprintf("%s_%s", name, tag)
}
