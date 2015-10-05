test_that("tbl_georeference_latlon works", {
  pg = src_postgres("dplyrpggeo")

  get_data = function(n) {
    data = tbl_df(data.frame(
      lat = runif(n, -90, 90),
      lon = runif(n, -180, 180)
    ))
  }

  x = tbl_cache(pg, "xy", get_data, 1000, tag=1, temporary = TRUE) %>%
    tbl_georeference_latlon()

  expect_true( "loc" %in% colnames(x) )
})
