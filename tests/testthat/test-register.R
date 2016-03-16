context("measurement registering")

test_that("register measurement works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  ms_obj <- epivizrData:::register(gr)
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizBlockData")
  expect_is(ms_obj$.object, "GNCList")
  
  newgr <- as(ms_obj$.object, "GRanges")
  mcols(newgr) <- mcols(ms_obj$.object)
  expect_equal(newgr, gr)
  
  expect_true(is.null(ms_obj$.columns))
})