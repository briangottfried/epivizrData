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

test_that("register works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1),score=rnorm(10))
  ms_obj <- epivizrData::register(gr, columns="score", type="bp")
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizBpData")
  expect_is(ms_obj$.object, "GNCList")
  
  newgr <- as(ms_obj$.object, "GRanges")
  mcols(newgr) <- mcols(ms_obj$.object)
  
  expect_equal(newgr, unname(gr))
  expect_equal(ms_obj$.columns, "score")
  rng <- range(pretty(range(gr$score)))
  expect_equal(ms_obj$.ylim, unname(cbind(rng)))
})