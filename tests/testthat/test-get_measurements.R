context("get_measurements from data objects")

test_that("get_measurements works for blocks", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  ms_obj <- epivizrData::register(gr)
  ms_id <- ms_obj$get_id()
  ms_name <- ms_obj$get_source_name()
  ms <- ms_obj$get_measurements()

  exp_ms <- list(
    list(
      id = ms_id,
      name = ms_obj$.name,
      type = "range",
      datasourceId = ms_id,
      datasourceGroup = ms_id,
      datasourceName = ms_name,
      defaultChartType = "BlocksTrack",
      dataprovider = character(),
      annotation = NULL,
      minValue = as.numeric(NA),
      maxValue = as.numeric(NA),
      metadata = NULL)
  )
  obs <- lapply(ms, as.list)
  expect_equal(obs, exp_ms)
})

test_that("get_measurements works for bp", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  ms_obj <- epivizrData::register(gr, type="bp")
  ms_id <- ms_obj$get_id()
  ms_name <- ms_obj$get_source_name() 
  rngs <- sapply(1:2, function(i) range(pretty(range(mcols(gr)[,paste0("score",i)], na.rm=TRUE))))
    
  exp_ms <- lapply(1:2, function(i) {
      list(
        id=paste0("score",i),
        name=paste0("score",i),
        type="feature",
        datasourceId=ms_id,
        datasourceGroup=ms_id,
        datasourceName=ms_name,
        defaultChartType="LineTrack",
        dataprovider=character(),
        annotation=NULL,
        minValue=rngs[1,i],
        maxValue=rngs[2,i],
        metadata=NULL)
  })

  obs_ms <- ms_obj$get_measurements()
  expect_equal(lapply(obs_ms, as.list), exp_ms)
})

test_that("get_measurements works for RangedSummarizedExperiment", {
  sset <- make_test_SE()
  ms_obj <- epivizrData::register(sset, columns=c("A","B"), assay="counts2")
  ms_id <- ms_obj$get_id()
  ms_name <- ms_obj$get_source_name()
  
  rngs <- unname(sapply(c("A","B"), function(col) range(pretty(range(assay(sset,"counts2")[,col], na.rm=TRUE)))))
    
  exp_ms <- lapply(c("A","B"), function(col) {
      i <- match(col,c("A","B"))
      list(
        id=col,
        name=col,
        type="feature",
        datasourceId=ms_id,
        datasourceGroup=ms_id,
        datasourceName=ms_name,
        defaultChartType="ScatterPlot",
        dataprovider=character(),
        annotation=list(Treatment=as.character(colData(sset)[i,])),
        minValue=rngs[1,i],
        maxValue=rngs[2,i],
        metadata=c("probeid"))
    })

  obs_ms <- ms_obj$get_measurements()
  expect_equal(lapply(obs_ms, as.list), exp_ms)
})

test_that("get_measurements works for ExpressionSet", {
  skip_if_not_installed("hgu133plus2.db")
  
  eset <- make_test_eset()
  ms_obj <- epivizrData::register(eset, columns=c("SAMP_1","SAMP_2"))
  ms_id <- ms_obj$get_id()
  ms_name <- ms_obj$get_source_name()
  
  rngs <- sapply(1:2, function(i) range(pretty(range(exprs(eset)[,paste0("SAMP_",i)]))))
    
  exp_ms <- lapply(1:2, function(i) {
    list(id=paste0("SAMP_",i),
      name=paste0("SAMP_",i),
      type="feature",
      datasourceId=ms_id,
      datasourceGroup=ms_id,
      datasourceName=ms_name,
      defaultChartType="ScatterPlot",
      dataprovider=character(),
      annotation=list(a=as.character(pData(eset)[i,1]), 
                      b=as.character(pData(eset)[i,2])),
      minValue=rngs[1,i],
      maxValue=rngs[2,i],
      metadata=c("PROBEID","SYMBOL"))
  })

  obs_ms <- ms_obj$get_measurements()
  expect_equal(lapply(obs_ms, as.list), exp_ms)
})

test_that("get_measurements works for gene info gr", {
  skip_if_not_installed("bumphunter")
  gr <- make_test_gene_info()
  ms_obj <- epivizrData::register(gr, type="gene_info")
  ms_id <- ms_obj$get_id()
  ms_name <- ms_obj$get_source_name()
  
  exp_ms <- list(list(id=ms_id,
                 name=ms_obj$.name,
                 type = "range",
                 datasourceId = ms_id,
                 datasourceGroup = ms_id,
                 datasourceName = ms_name,
                 defaultChartType = "GenesTrack",
                 dataprovider=character(),
                 annotation = NULL,
                 minValue = as.numeric(NA),
                 maxValue = as.numeric(NA),
                 metadata = c("gene", "exon_starts", "exon_ends")))
  obs_ms <- ms_obj$get_measurements()
  expect_equal(lapply(obs_ms, as.list), exp_ms)
})

