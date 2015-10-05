# cat inst/sql/postgis2gmap/postgis2gmap.sql | psql dplyrpggeo

test_that("tbl_tilereference works", {
  pg = src_postgres("dplyrpggeo")

  get_data = function(n) {
    tbl_df(data.frame(
      lat = runif(n, -90, 90),
      lon = runif(n, -180, 180)
    ))
  }

  x = tbl_cache(pg, "xy", get_data, 1000, tag=1, temporary = TRUE) %>%
    tbl_georeference_latlon() %>%
    tbl_tilereference()

  expect_true( all(c("xindex", "yindex", "level") %in% colnames(x)) )
})
