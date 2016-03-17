context("data fetch")

test_that("block data fetch works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  ms_obj <- epivizrData::register(gr)
  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2, end=6))
  res <- ms_obj$get_rows(query, character())
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=2:6,
                start=2:6,
                end=2:6,
                metadata=NULL))
  expect_equal(res, out)
})

test_that("msmt fetch works on unsorted data", {
  skip("for now")
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=10:1, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  msObj1 <- epivizr::register(gr1)

  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2, end=6))
  res <- msObj1$getRows(query,NULL)
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=2:6,
                start=2:6,
                end=2:6,
                metadata=(NULL)))
  expect_equal(res,out)
})

test_that("device data fetch works on bp data", {
  skip("for now")
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  msObj1 <- epivizr::register(gr3, type="bp")
  expect_is(msObj1, "EpivizBpData")

  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2,end=6))
  res <- msObj1$getRows(query, NULL)
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=list(2),
                start=list(6),
                end=list(6),
                metadata=NULL))

  expect_equal(res, out)
  #print(res);print(out)
  res <- msObj1$getValues(query, c("score1"))
  out <- list(globalStartIndex=2,
              values=list(6))
  expect_equal(res,out)  
})

test_that("device data fetch works on bp data with NAs", {
  skip("for now")
 gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
 gr3$score2[1:10]=NA

 msObj1 <- epivizr::register(gr3, type="bp")
 expect_is(msObj1, "EpivizBpData")
 query <- GRanges("chr1", IRanges(start=2, end=6))
 dataPack <- EpivizBpData$new()$.initPack(2L)
 dataPack$set(msObj1$getData(query=query,
                             msId="bp1$score1"),
              msId="bp1$score1",
              index=1)
 dataPack$set(msObj1$getData(query=query,
                             msId="bp1$score2"),
              msId="bp1$score2",
              index=2)
 res <- dataPack$getData()

 out=list()
 lims <- cbind(range(pretty(seq(1,96,len=10))),
               range(pretty(seq(-96,-51,len=10))))
 out$min=structure(lims[1,], names=paste0("bp1$score", 1:2))
 out$max=structure(lims[2,], names=paste0("bp1$score", 1:2))
 out$data=structure(list(list(bp=6,value=6),list(bp=integer(),value=numeric())), names=paste0("bp1$score", 1:2))

  # cat("res\n"); print(res)
  # cat("out\n"); print(out)

expect_equal(res,out)
})


test_that("feature data fetch works", {
  skip("for now")
  eset <- makeEset()
  msObj <- epivizr::register(eset, columns=c("SAMP_1", "SAMP_2"))
  query <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000,end=40000000))

  olaps <- findOverlaps(query, msObj$object)
  hits <- unique(subjectHits(olaps))
  hits <- seq(min(hits),max(hits))
  tmp <- msObj$object[hits,]

  m <- match(rowRanges(tmp)$PROBEID, featureNames(eset))
  mat <- exprs(eset)[m, c("SAMP_1", "SAMP_2")]

  res <- msObj$getRows(query, c("PROBEID","SYMBOL"))
  
  out <- list(globalStartIndex=min(hits),
              useOffset=FALSE,
              values=list(
                id=hits,
                start=start(tmp),
                end=end(tmp),
                metadata=list(PROBEID=rowRanges(tmp)$PROBEID,
                  SYMBOL=rowRanges(tmp)$SYMBOL)
                ))
  expect_equal(res, out)
  #print(res); print(out)
  
  res <- msObj$getValues(query, "SAMP_1")
  out <- list(globalStartIndex=min(hits),
              values=unname(mat[,"SAMP_1"]))
  #print(res);print(out)
  expect_equal(res,out)
})

test_that("geneinfo fetch works", {
  skip("for now")
  sendRequest <- sendRequest
  gr <- makeGeneInfo()
  msmt <- epivizr::register(gr, type="geneInfo")
  query <- GRanges("chr11", IRanges(start=102500000, end=103000000))
  res <- msmt$getRows(query, c("gene", "exon_starts", "exon_ends"))
  
  msGR <- msmt$object
  olaps <- findOverlaps(query, msGR)
  hits <- subjectHits(olaps)
  hits <- seq(min(hits), max(hits))
  tmp <- msGR[hits,]
  
  out <- list(globalStartIndex=hits[1],
              useOffset=FALSE,
              values=list(
                id=hits,
                start=start(tmp),
                end=end(tmp),
                metadata=list(gene=unname(as.character(tmp$Gene)),
                              exon_starts=unname(lapply(start(tmp$Exons),paste,collapse=",")),
                              exon_ends=unname(lapply(end(tmp$Exons), paste, collapse=","))),
                strand=unname(as.character(strand(tmp)))))
  #print(res); print(out)
  expect_equal(res, out)
})

