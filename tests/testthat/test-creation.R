context("object creation")

test_that("new creates a proper object", {
  server <- epivizrServer::createServer()
  mgr <- EpivizDataMgr$new(server=server)
  expect_is(mgr, "EpivizDataMgr")
  
  expect_is(mgr$.ms_list, "list")
  expect_equal(length(mgr$.msList), 0)
  
  expect_equal(mgr$.ms_idCounter, 0)
  
  expect_is(mgr$.server, "EpivizServer")
  expect_true(mgr$is_closed())
})

