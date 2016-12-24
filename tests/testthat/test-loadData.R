context("Load data funtions")
#' @examples {
#' devtools::test(filter='LoadData')
#' }


test_that("function loads",{

  expect_true(
    is.character(make_filename(2012)) , 'make_filename returns string')

})
