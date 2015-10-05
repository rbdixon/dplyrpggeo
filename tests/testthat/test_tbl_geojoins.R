get_coords = function(pg) {
  tbl_cache(pg, "coords", function() {expand.grid(lat=-1:1, lon=-1:1)}, temporary=TRUE, volatile=TRUE) %>%
    tbl_georeference_latlon()
}


test_that("tbl_tilereference works", {
  pg = src_postgres("dplyrpggeo")
  coords = get_data(pg)


  expect_true( TRUE )
})
