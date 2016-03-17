EpivizFeatureData <- setRefClass("EpivizFeatureData",
  contains="EpivizData",
  fields=list(.assay="ANY", .metadata="ANY"),
  methods=list(
    initialize=function(object = SummarizedExperiment(matrix(nr=0,nc=0),
                                                      rowRanges=GRanges()),
                        assay=1, metadata = NULL, ...) {
      .self$.assay <- assay
      .self$.metadata <- metadata
      callSuper(object=object, ...)
    },
    # update=function(newObject, ...) {
    #   if (!is(newObject, "RangedSummarizedExperiment"))
    #     stop("'newObject' must be of class 'RangedSummarizedExperiment'")
    # 
    #   newObject <- reorderIfNecessary(newObject)
    #   
    #   if(!is(rowRanges(newObject), "GNCList"))
    #     rowRanges(newObject) <- as(rowRanges(newObject), "GNCList")
    #   callSuper(newObject, ...)
    # },
    .check_columns = function(columns) {
      all(columns %in% rownames(colData(.self$.object)))
    },
    .infer_columns = function() {
      rownames(colData(.self$.object))
    },
    .check_limits = function(ylim) {
      if (!is.matrix(ylim))
        return(FALSE)
      if (nrow(ylim) != 2)
        return(FALSE)
      if (ncol(ylim) != length(.self$.columns))
        return(FALSE)
      TRUE
    },
    .infer_nas = function() {
      mat <- SummarizedExperiment::assay(.self$.object, i=.self$.assay)
      col_index <- match(.self$.columns, rownames(colData(.self$.object)))
      na_mat <- is.na(mat[,col_index])
      if (!is.matrix(na_mat))
        na_mat <- cbind(na_mat)
      which(rowSums(na_mat)>0)
    },
    .infer_limits = function() {
      mat <- SummarizedExperiment::assay(.self$.object, i=.self$.assay)
      col_index <- match(.self$.columns, rownames(colData(.self$.object)))
      unname(sapply(col_index, function(i) range(pretty(range(mat[,i], na.rm=TRUE)))))
    }#,
    # plot=function(x, y, ...) {
    #   ms <- getMeasurements()
    #   if (length(ms)<2)
    #     stop("need at least two columns to plot")
    # 
    #   mgr$scatterChart(x=ms[[1]], y=ms[[2]], ...)
    # }
  )
)

.valid.EpivizFeatureData.object <- function(x) {
  if(!is(x$.object, "RangedSummarizedExperiment"))
    return("'object' must be of class 'RangedSummarizedExperiment'")
  if(!is(rowRanges(x$.object), "GNCList"))
    return("'rowRanges(object)' must be of class 'GNCList'")
  NULL
}

.valid.EpivizFeatureData.ylim <- function(x) {
  if(!is(x$.ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$.ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$.ylim) != length(x$.columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizFeatureData.assay <- function(x) {
  if (is.character(x$.assay)) {
    if(!(x$.assay %in% names(assays(x$.object))))
      return("'assay' not found in 'object'")
    return(NULL)
  }

  if (x$.assay > length(assays(x$.object)))
    return("'assay' not found in 'object'")
  NULL
}

.valid.EpivizFeatureData <- function(x) {
  c(.valid.EpivizFeatureData.object(x),
    .valid.EpivizFeatureData.ylim(x),
    .valid.EpivizFeatureData.assay(x))
}

S4Vectors::setValidity2("EpivizFeatureData", .valid.EpivizFeatureData)

EpivizFeatureData$methods(
    get_measurements=function() {
      out <- lapply(.self$.columns, function(cur_col) {
        m <- match(cur_col, .self$.columns)

        anno <- NULL
        if (ncol(colData(.self$.object)) > 0) {
          anno <- as.list(colData(.self$.object)[cur_col,,drop=FALSE])
          anno <- lapply(anno, as.character)          
        }

        list(id=cur_col,
           name=cur_col,
           type="feature",
           datasourceId=.self$.id,
           datasourceGroup=.self$.id,
           defaultChartType="Scatter Plot",
           annotation=anno,
           minValue=.self$.ylim[1,m],
           maxValue=.self$.ylim[2,m],
           metadata=.self$.metadata)
    })

      out
    }
)#,
#     parseMeasurement=function(msId) {
#       column <- strsplit(msId, split="__")[[1]][2]
#       if(!.checkColumns(column)) {
#         stop("invalid parsed measurement")
#       }
#       column
#     },
#     .getMetadata=function(curHits, curMetadata) {
#       if (length(metadata) < 1) {
#           return(NULL)
#       }
# 
#       if(any(!curMetadata %in% metadata))
#         stop("error getting metadata")
# 
#       if (length(curHits) == 0) {
#         out <- lapply(curMetadata, function (x) list())
#         names(out) <- curMetadata
#         return(out)
#       }
#       out <- as.list(mcols(rowRanges(object))[curHits,curMetadata])
#       names(out) <- curMetadata
#       out
#     },
#   .getValues=function(curHits, measurement, round=FALSE) {
#     if (!measurement %in% columns) {
#       stop("could not find measurement", measurement)
#     }
#     colNames <- colnames(object)
#     m <- match(measurement, colNames)
#     unname(assay(object, .self$assay)[curHits, m])
#   }
# )
# 
