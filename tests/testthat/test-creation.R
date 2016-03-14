context("object creation")

test_that("new creates a proper object", {
  server <- epivizrServer::createServer()
  mgr <- EpivizDataMgr$new(server=server)
  expect_is(mgr, "EpivizDataMgr")
  
  expect_is(mgr$.ms_list, "list")
  expect_equal(length(mgr$.ms_list), 0)
  
  expect_equal(mgr$.ms_idCounter, 0)
  
  expect_is(mgr$.server, "EpivizServer")
  expect_true(mgr$is_server_closed())
})

test_that("server opening works as expected", {
  server <- epivizrServer::createServer()
  mgr <- EpivizDataMgr$new(server=server)
  expect_true(mgr$is_server_closed())
  
  server$start_server()
  on.exit(server$stop_server())
  
  expect_false(mgr$is_server_closed())
})

test_that("type registration works", {
  server <- epivizrServer::createServer()
  mgr <- EpivizDataMgr$new(server=server)
  
  descriptor <- list(class="EpivizData",
    description="A phony data type",
    input_class="GRanges")

  mgr$register_type("data_type", descriptor)
  expect_equal(length(mgr$.ms_list), 1)
  expect_false(is.null(mgr$.ms_list[["data_type"]]))
  expect_is(mgr$.ms_list[["data_type"]], "environment")
  
  expect_equal(length(mgr$.type_map), 1)
  expect_false(is.null(mgr$.type_map[["data_type"]]))
  expect_equal(mgr$.type_map[["data_type"]], descriptor)
  
  expect_error(mgr$register_type("another_type", list()))
})