# cat inst/sql/postgis2gmap/postgis2gmap.sql | psql dplyrpggeo

test_that("tbl_georeference_tile works", {
  pg = src_postgres("dplyrpggeo")

  get_data = function(n) {
    tbl_df(data.frame(
      lat = runif(n, -90, 90),
      lon = runif(n, -180, 180)
    ))
  }

  x = tbl_cache(pg, "xy", get_data, 1000, tag=1) %>%
#     tbl_georeference_latlon() %>%
#     tbl_tilereference() %>%
#     tbl_georeference_tile()
    identity

  expect_true( all(c("tileloc", "tilebounds") %in% colnames(x)) )
})
