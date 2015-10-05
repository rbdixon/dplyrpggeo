# $ createdb dplyrpggeo
# $ psql dplyrpggeo -c "create extension postgis"

data(iris)

test_that("tbl_cache actually caches", {
  pg = src_postgres("dplyrpggeo")

  cfn = function() { return(iris) }

  ref = tbl_cache(pg, "iris", cfn)
  name = as.character( ref$from )

  x = collect( ref )

  # If this function is executed then the cache failed
  cfn = function() { stop("Shouldn't run!") }

  y = collect( tbl_cache(pg, "iris", cfn) )

  # Results should be equal, of course!
  expect_equal( iris, x )
  expect_equal( iris, y )

  # Clean up
  db_drop_table(pg$con, name)

})

test_that("tbl_cache temporary does not leave trash", {
  pg = src_postgres("dplyrpggeo")

  cfn = function() { return(iris) }
  ref = tbl_cache(pg, "iris", cfn, temporary=TRUE)
  name = as.character( ref$from )

  # Test to see if table is temporary
  # Don't have a test for this, yet!
  expect_true(TRUE)

  # Clean up
  db_drop_table(pg$con, name)
})

test_that("tbl_cache indexes build without error", {
  pg = src_postgres("dplyrpggeo")

  get_data = function(n) {
    data = tbl_df(data.frame(
      lat = runif(n, -90, 90),
      lon = runif(n, -180, 180)
    ))
  }

  x = tbl_cache(pg, "xy", get_data, 1000, tag=1, indexes = list("lat", "lon"))
  name = as.character( x$from )

  expect_true(TRUE)
  db_drop_table(pg$con, name)
})

test_that("tbl_cache geoindexes build without error", {
  pg = src_postgres("dplyrpggeo")

  get_data = function(pg, n) {
    data = tbl_df(data.frame(
      lat = runif(n, -90, 90),
      lon = runif(n, -180, 180)
    )) %>%
      copy_to(pg, ., name="t1", temporary=TRUE) %>%
      tbl_georeference_latlon()
  }

  x = tbl_cache(pg, "xy", get_data, pg, 1000, tag=1, indexes = list("lat", "lon"), geoindexes = list("loc"))
  name = as.character( x$from )

  expect_true(TRUE)
  db_drop_table(pg$con, name)
})
