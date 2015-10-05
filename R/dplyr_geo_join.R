#' Join two tbls together geometrically based on proximity.
#'
#' These generic functions are only supported on PostgreSQL with the PostGIS extensions for
#' working with geospatial data.
#' @inheritParams dplyr::inner_join
#' @param within join geometries of \code{x} and \code{y} if the geometries are within the
#' specified \code{within} meters of one another. If \code{distance==0} then geometries must intersect
#' to be joined.
#' @seealso \code{\link[dplyr]{inner_join}} for details on dplyr join functions. These PostGIS
#' functions are used: \href{http://postgis.net/docs/ST_DWithin.html}{ST_DWithin()} for proximity
#' joins when \code{within!=0} and \href{http://postgis.net/docs/ST_Contains.html}{ST_Contains()} for
#' intersection when \code{within==0}.
#' @name geo_join
NULL

#' @rdname geo_join
#' @export
inner_geo_join <- function(x, y, by = NULL, within = 0, ...) {
  UseMethod("inner_geo_join")
}

#' @rdname geo_join
#' @export
left_geo_join <- function(x, y, by = NULL, within = 0, ...) {
  UseMethod("left_geo_join")
}

#' @rdname geo_join
#' @export
right_geo_join <- function(x, y, by = NULL, within = 0, ...) {
  UseMethod("right_geo_join")
}

#' @rdname geo_join
#' @export
full_geo_join <- function(x, y, by = NULL, within = 0, ...) {
  UseMethod("full_geo_join")
}

#' @rdname geo_join
#' @export
semi_geo_join <- function(x, y, by = NULL, within = 0, ...) {
  UseMethod("semi_geo_join")
}

environment(inner_geo_join) = asNamespace("dplyr")
environment(left_geo_join) = asNamespace("dplyr")
environment(right_geo_join) = asNamespace("dplyr")
environment(full_geo_join) = asNamespace("dplyr")
environment(semi_geo_join) = asNamespace("dplyr")

#
# FROM dplyr tbl-sql.R
#

inner_geo_join.tbl_sql <- function(x, y, by = NULL,
                               auto_index = FALSE, within = 0, ...) {
  by <- common_by(by, x, y)
  sql <- sql_geo_join(x$src$con, x, y, type = "inner", by = by, within = within)
  update(tbl(x$src, sql), group_by = groups(x))
}

left_geo_join.tbl_sql <- function(x, y, by = NULL,
                              auto_index = FALSE, within = 0, ...) {
  by <- common_by(by, x, y)
  sql <- sql_geo_join(x$src$con, x, y, type = "left", by = by, within = within)
  update(tbl(x$src, sql), group_by = groups(x))
}

right_geo_join.tbl_sql <- function(x, y, by = NULL,
                                  auto_index = FALSE, within = 0, ...) {
  by <- common_by(by, x, y)
  sql <- sql_geo_join(x$src$con, x, y, type = "right", by = by, within = within)
  update(tbl(x$src, sql), group_by = groups(x))
}

full_geo_join.tbl_sql <- function(x, y, by = NULL,
                                  auto_index = FALSE, within = 0, ...) {
  by <- common_by(by, x, y)
  sql <- sql_geo_join(x$src$con, x, y, type = "full", by = by, within = 0)
  update(tbl(x$src, sql), group_by = groups(x))
}

semi_geo_join.tbl_sql <- function(x, y, by = NULL,
                                  auto_index = FALSE, within = 0, ...) {
  by <- common_by(by, x, y)
  sql <- sql_geo_join(x$src$con, x, y, type = "semi", by = by, within = 0)
  update(tbl(x$src, sql), group_by = groups(x))
}

environment(inner_geo_join.tbl_sql) = asNamespace("dplyr")
environment(left_geo_join.tbl_sql) = asNamespace("dplyr")
environment(right_geo_join.tbl_sql) = asNamespace("dplyr")
environment(full_geo_join.tbl_sql) = asNamespace("dplyr")
environment(semi_geo_join.tbl_sql) = asNamespace("dplyr")

#
# FROM dplyr dbi-s3.r
#

sql_geo_join <- function(con, x, y, type = "inner", by = NULL, within = 0, ...) {
  join <- switch(type,
                 left = sql("LEFT"),
                 inner = sql("INNER"),
                 right = sql("RIGHT"),
                 full = sql("FULL"),
                 semi = sql("SEMI"),
                 stop("Unknown join type:", type, call. = FALSE)
  )

  by <- common_by(by, x, y)
  using <- all(by$x == by$y)

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select)
  uniques <- unique_names(x_names, y_names, by$x[by$x == by$y])

  if (is.null(uniques)) {
    sel_vars <- c(x_names, y_names)
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])

    sel_vars <- unique(c(uniques$x, uniques$y))
  }

  if (using) {
    stop("by parameter is required")
  } else {
    if (within == 0) {
      on <- sql_vector(paste0("ST_Contains(", sql_escape_ident(con, by$x), "::geometry , ", sql_escape_ident(con, by$y), "::geometry )" ),
                       collapse = " AND ", parens = TRUE)
    } else {
      on <- sql_vector(paste0("ST_DWithin(", sql_escape_ident(con, by$x), ", ", sql_escape_ident(con, by$y), ", ", within, ")" ),
                       collapse = " AND ", parens = TRUE)
    }
    cond <- build_sql("ON ", on, con = con)
  }

  from <- build_sql(
    'SELECT * FROM ',
    sql_subquery(con, x$query$sql), "\n\n",
    join, " JOIN \n\n" ,
    sql_subquery(con, y$query$sql), "\n\n",
    cond, con = con
  )
  attr(from, "vars") <- lapply(sel_vars, as.name)
  from
}

environment(sql_geo_join) = asNamespace("dplyr")

