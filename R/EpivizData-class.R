#' Data container for epiviz data server
#' 
#' @docType class
#' @import methods
#' @import S4Vectors
EpivizData <- setRefClass("EpivizData",
  contains="VIRTUAL",
  fields=list(
    .object="ANY",
    .mgr="EpivizDataMgr",
    .id="character",
    .name="character",
    .columns="ANY",
    .ylim="ANY",
    .curQuery="ANY",
    .curHits="ANY"
  ),
  methods=list(
    initialize = function(object = GNCList(GRanges()), 
                          columns=NULL, 
                          ylim=NULL, ...) {
      .self$.object <- object
      .self$.columns <- columns

      if (!.self$.check_columns(columns))
        stop("Invalid 'columns' argument")

      if (is.null(.self$.columns))
        .self$.columns <- .self$.infer_columns()

      naIndex <- .self$.infer_nas()
      if (length(naIndex)>0) {
        .self$.object <- .self$.object[-naIndex,]
      }
      
      if (!is.null(ylim)) {
        if (!.self$.check_limits(ylim))
          stop("invalid 'ylim' argument")
        .self$.ylim <- ylim
      } else {
        .self$.ylim <<- .self$.infer_limits()
      }

      .self$.curQuery <- NULL
      .self$.curHits <- NULL
      callSuper(...)
    },
    .infer_nas = function() {
      integer()
    },
    .check_columns = function(columns) {
      is.null(columns)
    },
    .infer_columns = function() {
      NULL
    },
    .check_limits = function(ylim) {
      is.null(ylim)
    },
    .infer_limits = function() {
      NULL
    },
    .check_class = function() {
      TRUE
    },
    update = function(new_object, send_request=TRUE) {
      "Update underlying data object with new object"
      # TODO: use a method to check new object is of valid class
      #       so subclasses can overload and check appropriately
      if (!.self$check_class(new_object)) {
        stop("class of 'new_object' is not equal to class of current 'object'")
      }

      original_object <- .self$.object
      .self.$.object <- newObject

      if (!is.null(columns)) {
        if (!.self$.check_columns(columns)) {
          .self$.object <- original_object
          stop("columns not found in 'newObject'")
        }

        .self$.ylim <- .self$.infer_limits()
      }


      # TODO: make sure object is of the proper type for efficient querying      
      #if(is(object,"RangedSummarizedExperiment") && !is(rowRanges(object),"GIntervalTree")) {
       # rowRanges(object) <<- as(rowRanges(object), "GIntervalTree")
      #}

      na_index <- .self$.infer_nas()
      if (length(na_index) > 0) {
        .self$.object <- .self$.object[-na_index,]
      }
      
      if (send_request && !is.null(.self$.mgr) && !.self$.mgr$is_closed())
        .self$.mgr$.clear_datasourceGroup_cache(.self, send_request=send_request)
      invisible()
    },
    # TODO: use accessor functions for this
    get_id = function() {
      .self$.id
    },
    set_id = function(id) {
      .self$.id <- id
      invisible()
    },
    # TODO: use accessor functions for this
    get_name = function() { .self$.name },
    set_name=function(name) {
      .self$.name <- name
      invisible()
    },
    set_limits = function(ylim) {
      if (!.self$.check_limits(ylim))
          stop("'invalid' limits argument")
      .self$.ylim <- ylim
    }, 
    get_measurements = function() {
      stop("'get_measurements' called on virtual class object")
    },
    parse_measurement = function(ms_id=NULL) {
      stop("'parse_measurement' called on virtual class object")
    },
    set_mgr = function(mgr) {
      if (!is(mgr, "EpivizDataMgr"))
        stop("'mgr' must be of class 'EpivizDataMgr'")
      
      .self$.mgr <- mgr
      invisible()
    },
    show = function() {
      cat(class(.self), "object", .self$.id, "\n")
      methods::show(.self$.object)
      cat("\n\tcolumns:", paste(.self$.columns, collapse=","),"\n")
      cat("\tlimits:\n")
      print(.self$.ylim)
    }
  )
)

#####
# validity
.valid.EpivizData.columns <- function(x) {
  if(!x$.check_columns(x$.columns))
    return("invalid 'columns' slot")
  NULL
}

.valid.EpivizData <- function(x) {
  c(.valid.EpivizData.columns(x))
}

S4Vectors::setValidity2("EpivizData", .valid.EpivizData)

#######
# get data
# EpivizData$methods(
#   getHits=function(query) {
#     if (!is(query, "GRanges"))
#       stop("'query' must be a GRanges object")
#     if (length(query) != 1) {
#       stop("'query' must be of length 1")
#     }
# 
#     if (is.null(curQuery) || !identical(unname(query), unname(curQuery))) {
#       curQuery <<- query
#       olaps <- suppressWarnings(GenomicRanges::findOverlaps(query, object, select="all"))
#       curHits <<- subjectHits(olaps)
# 
#       if (length(curHits) == 0) {
#         return(invisible())
#       }
#       
#       if (!S4Vectors::isSorted(start(object)[curHits])) {
#         stop("these should be ordered by now...")
#      }
#       curHits <<- seq(min(curHits), max(curHits))
#     }
#     invisible()
#   },
#   getRows=function(query, metadata, useOffset=FALSE) {
#     if (is.null(query)) {
#       out <- list(globalStartIndex=NULL, useOffset=FALSE,
#                   values=list(id=list(),
#                               start=list(),
#                               end=list(),
#                               metadata=.self$.getMetadata(integer(), metadata)))
#       return(out)
#     }
#     
#     getHits(query)
#     if (length(curHits) == 0) {
#       out <- list(globalStartIndex=NULL, useOffset=FALSE,
#                   values=list(id=list(),
#                     start=list(),
#                     end=list(),
#                     metadata=.self$.getMetadata(curHits, metadata)))
#     } else {
#       if (!useOffset) {
#         out <- list(globalStartIndex=curHits[1],
#                   useOffset=FALSE,
#                   values=list(
#                     id=curHits,
#                     start=start(object)[curHits],
#                     end=end(object)[curHits],
#                     metadata=.self$.getMetadata(curHits, metadata)
#                    ))
#       } else {
#         st <- start(object)[curHits]
#         stDiff <- diff(st)
#         end <- end(object)[curHits]
#         endDiff <- diff(end)
#         
#         out <- list(globalStartIndex=curHits[1],
#                     useOffset=TRUE,
#                     values=list(
#                       id=curHits,
#                       start=c(st[1], stDiff),
#                       end=c(end[1],endDiff),
#                       metadata=.self$.getMetadata(curHits, metadata)
#                      ))
#         }
#     }
#     if (length(out$values)>0 && length(out$values$id) == 1) {
#       for (slotName in names(out$values)) {
#         # TODO: switch to simplejson
#         if (slotName != "metadata")
#           out$values[[slotName]] <- list(out$values[[slotName]])
#       }
#     }
#     return(out)
#   },
#   .getValues=function(curHits, measurement, round) {
#     numeric()
#   },
#   getValues=function(query, measurement, round=TRUE) {
#     if (is.null(query)) {
#       out <- list(globalstartIndex=NULL, values=list())
#       return(out)
#     }
#     
#     getHits(query)
#     if (length(curHits) == 0) {
#       out <- list(globalStartIndex=NULL, values=list())
#     } else {
#       out <- list(globalStartIndex=curHits[1],
#                   values=.self$.getValues(curHits, measurement, round=round))
#       if (length(out$values) ==1) {
#         out$values <- list(out$values)
#       }
#     }
#     return(out)
#   }
# )
# 
# 
# 
