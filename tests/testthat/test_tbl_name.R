test_that("tbl_name returns table name", {
  expect_equal( class(tbl_name()), "character" )
  expect_more_than( length(tbl_name()), 0 )
})

test_that("custom name and tag works", {
  expect_equal( tbl_name("a", "b"), "a_b")
})
