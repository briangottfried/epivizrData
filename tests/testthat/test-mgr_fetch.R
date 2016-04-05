context("manager fetch")

test_that("disconnected mgr fetch works", {
  skip_if_not_installed("hgu133plus2.db")
  gr1 <- GRanges(seqnames="chr6", 
                 ranges=IRanges::IRanges(start=30000000+(1:10), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr7", 
                 ranges=IRanges::IRanges(start=30000000+(2:20), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr6", 
                 ranges=IRanges::IRanges(start=30000000+seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  eset <- make_test_eset()
  

  server <- epivizrServer::createServer()
  mgr <- createMgr(server)
  
  dev1 <- mgr$add_measurements(gr1, "dev1", send_request=FALSE); devId1=dev1$get_id()
  dev2 <- mgr$add_measurements(gr2, "dev2", send_request=FALSE); devId2=dev2$get_id()
  dev3 <- mgr$add_measurements(gr3, "dev3", send_request=FALSE, type="bp"); devId3=dev3$get_id()
  dev4 <- mgr$add_measurements(eset, "dev4", send_request=FALSE, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$get_id()
    
  m <- match(rowRanges(dev4$.object)$PROBEID, featureNames(eset))
  mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
  lims <- unname(apply(mat, 2, function(x) range(pretty(range(x)))))
    
  query <- GRanges(seqnames="chr6", ranges=IRanges::IRanges(start=30000000,end=40000000))
    
  hits <- unique(subjectHits(findOverlaps(query, dev4$.object)))
  hits <- seq(min(hits), max(hits))
  tmp <- dev4$.object[hits,]
    
  m <- match(rowRanges(tmp)$PROBEID, featureNames(eset))
  mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
    
  # dev 1
  res <- mgr$get_rows(seqnames(query), start(query), end(query), NULL, devId1)
  out <- list(globalStartIndex=1,
              useOffset=FALSE,
              values=list(
                id=1:10,
                start=30000000+(1:10),
                end=30000000+(100:109),
                metadata=NULL))
  expect_equal(res,out)

  # dev 2
  res <- mgr$get_rows(seqnames(query), start(query), end(query), NULL, devId2)
  out <- list(globalStartIndex=NULL,
              useOffset=FALSE,
              values=list(
                id=list(),
                start=list(),
                end=list(),
                metadata=NULL))
  expect_equal(res,out)

  # dev 3
  res <- mgr$get_rows(seqnames(query), start(query), end(query), NULL, devId3)
  out <- list(globalStartIndex=1,
              useOffset=FALSE,
              values=list(
                id=seq(len=length(seq(1,100,by=5))),
                start=30000000+seq(1,100,by=5),
                end=30000000+seq(1,100,by=5),
                metadata=NULL))
  expect_equal(res,out)

  res <- mgr$get_values(seqnames(query), start(query), end(query), devId3, "score1")
  out <- list(globalStartIndex=1,
              values=seq(1,100,by=5))
  expect_equal(res,out)

  res <- mgr$get_values(seqnames(query), start(query), end(query), devId3, "score2")
  out <- list(globalStartIndex=1,
              values=-seq(1,100,by=5))
  expect_equal(res,out)

  # dev 4
  res <- mgr$get_rows(seqnames(query), start(query), end(query), c("PROBEID","SYMBOL"), devId4)
  out <- list(globalStartIndex=hits[1],
              useOffset=FALSE,
              values=list(id=hits,
                          start=start(tmp),
                          end=end(tmp),
                          metadata=list(PROBEID=rowRanges(tmp)$PROBEID,
                                        SYMBOL=rowRanges(tmp)$SYMBOL)))
  expect_equal(res,out)

  res <- mgr$get_values(seqnames(query), start(query), end(query), devId4, "SAMP_1")
  out <- list(globalStartIndex=hits[1],
              values=unname(mat[,"SAMP_1"]))
  expect_equal(res,out)

  res <- mgr$get_values(seqnames(query), start(query), end(query), devId4, "SAMP_2")
  out <- list(globalStartIndex=hits[1],
              values=unname(mat[,"SAMP_2"]))
  expect_equal(res,out)
})

test_that("connected mgr fetch rows works", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  skip_if_not_installed("rjson")
  skip_if_not_installed("hgu133plus2.db")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server(static_site_path=".")
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  mgr <- createMgr(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())
  
  eset <- make_test_eset()
  
  ms_obj <- mgr$add_measurements(eset, "dev4", send_request=TRUE, columns=c("SAMP_1", "SAMP_2")); 
  ms_id <- ms_obj$get_id()
  wait_until(!mgr$.server$has_request_waiting())
  
  query <- GRanges(seqnames="chr6", ranges=IRanges::IRanges(start=30000000,end=40000000))
  hits <- unique(subjectHits(findOverlaps(query, ms_obj$.object)))
  hits <- seq(min(hits), max(hits))
  
  mgr$.server$register_action("getRows", function(request_data) {
    mgr$get_rows(request_data$seqName,
                 request_data$start,
                 request_data$end,
                 request_data$metadata, request_data$datasource)
  })

  inputEl <- remDr$findElement(using="id", "_rows_input_location")
  inputEl$sendKeysToElement(list(sprintf("%s:%d-%d", "chr6", 30000000, 40000000)))

  inputEl <- remDr$findElement(using="id", "_rows_input_metadata")
  inputEl$sendKeysToElement(list("PROBEID,SYMBOL"))

  inputEl <- remDr$findElement(using="id", "_rows_input_datasource")
  inputEl$sendKeysToElement(list(ms_id))

  buttonEl <- remDr$findElement(using="id", "_rows_input_btn")
  buttonEl$clickElement()
  Sys.sleep(1)

  outputEl <- remDr$findElement(using="id", "get_rows_output")
  rows_info <- outputEl$getElementText()[[1]]
  cat(rows_info, "\n")

  exp_rows_info <- list(
    globalStartIndex = hits[1],
    useOffset = FALSE,
    nhits = length(hits),
    metadata = list(PROBEID=length(hits), SYMBOL=length(hits))
  )
  exp_rows_info <- rjson::toJSON(exp_rows_info)
  cat(exp_rows_info, "\n")
  expect_equal(rows_info, exp_rows_info)
})

test_that("connected mgr fetch values works", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  skip_if_not_installed("rjson")
  skip_if_not_installed("hgu133plus2.db")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server(static_site_path=".")
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  mgr <- createMgr(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())
  
  eset <- make_test_eset()
  
  ms_obj <- mgr$add_measurements(eset, "dev4", send_request=TRUE, columns=c("SAMP_1", "SAMP_2")); 
  ms_id <- ms_obj$get_id()
  wait_until(!mgr$.server$has_request_waiting())
  
  query <- GRanges(seqnames="chr6", ranges=IRanges::IRanges(start=30000000,end=40000000))
  hits <- unique(subjectHits(findOverlaps(query, ms_obj$.object)))
  hits <- seq(min(hits), max(hits))
  
  mgr$.server$register_action("getValues", function(request_data) {
    mgr$get_values(request_data$seqName, request_data$start, request_data$end,
                   request_data$datasource, request_data$measurement)
  })
  
  inputEl <- remDr$findElement(using="id", "_values_input_location")
  inputEl$sendKeysToElement(list(sprintf("%s:%d-%d", "chr6", 30000000, 40000000)))
  
  inputEl <- remDr$findElement(using="id", "_values_input_measurement")
  inputEl$sendKeysToElement(list("SAMP_1"))

  inputEl <- remDr$findElement(using="id", "_values_input_datasource")
  inputEl$sendKeysToElement(list(ms_id))

  buttonEl <- remDr$findElement(using="id", "_values_input_btn")
  buttonEl$clickElement()
  cat("getValues form submitted\n")
  Sys.sleep(1)

  outputEl <- remDr$findElement(using="id", "get_values_output")
  values_info <- outputEl$getElementText()[[1]]
  cat(values_info, "\n")

  exp_values_info <- list(
    globalStartIndex=hits[1],
    nvalues=length(hits)
  )
  exp_values_info <- rjson::toJSON(exp_values_info)
  cat(exp_values_info, "\n")
  expect_equal(values_info, exp_values_info)
})
