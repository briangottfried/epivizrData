setClassUnion("EpivizServerOrNULL", c("EpivizServer", "NULL"))

#' Class providing data manager for epiviz app
#'
#' @docType class
#' @import methods
#' @importClassesFrom epivizrServer EpivizServer
#' 
#' 
EpivizDataMgr <- setRefClass("EpivizDataMgr",
  fields = list(
    .ms_list = "environment",
    .ms_idCounter = "integer",
    .server = "EpivizServerOrNULL"
  ),
  methods=list(
    initialize=function(server=NULL, ...) {
      .self$.server <- server
      .self$.ms_list <- new.env(parent=emptyenv())
      .self$.ms_idCounter <- 0L
    },
    num_datasources = function() { length(ls(.self$.ms_list)) },
    show = function() {
      "Print manager information to screen"
      cat("Epiviz data manager object:\n")
      cat("Server: ")
      .self$.server$show(); cat("\n")
 
#       st <- .self$list_measurements()
#       if (length(st)>0) {
#         cat("Measurements:\n")
#         print(st); cat("\n")
    },
    .check_server = function() {
      !is.null(.self$.server) && !.self$.server$is_closed()
    },
    is_server_closed = function() {
      "Check if underlying server is closed, <logical>"
      is.null(.self$.server) || .self$.server$is_closed()
    }
  )
)

EpivizDataMgr$methods(
  add_measurements = function(obj, datasource_name, send_request = TRUE, ...) {
    "register measurements in data manager"
    if (missing(datasource_name) || !is.character(datasource_name)) {
      stop("data source name is required")
    }
    ms_object <- epivizrData:::register(obj, ...)

    .self$.ms_idCounter <- .self$.ms_idCounter + 1L
    ms_id <- sprintf("%s_%d", datasource_name, .self$.ms_idCounter)
    ms_object$set_id(ms_id)
    ms_object$set_name(datasource_name)
    ms_object$set_mgr(.self)
    
    measurements <- ms_object$get_measurements()
    ms_record <- list(measurements=measurements,
                      name=datasource_name, 
                      obj=ms_object, 
                      connected=FALSE)
    assign(ms_id, ms_record, envir=.self$.ms_list)
    
    send_request <- .self$.check_server() && isTRUE(send_request)
    if (send_request) {
      callback <- function(response_data) {
        .self$.ms_list[[ms_id]]$connected <- TRUE
        cat("Measurement ", datasource_name, " added to application and connected\n")
      }
      request_data <- list(action="addMeasurements",
                           measurements=epivizrServer::json_writer(measurements))
      .self$.server$send_request(request_data, callback)
    }
    ms_object
  },
  get_measurements = function() {
    "get metadata about all measurements registered"
    out <- list(id=character(),
                name=character(),
                type=character(),
                datasourceId=character(),
                datasourceGroup=character(),
                defaultChartType=character(),
                annotation=list(),
                minValue=numeric(),
                maxValue=numeric(),
                metadata=list()
    )
      
    measurements <- list()
    ids <- ls(.self$.ms_list)
    if (length(ids)>0) {
      for (id in ids) {
        ms_record <- .self$.ms_list[[id]]
        ms <- ms_record$obj$get_measurements()
        for (cur_ms in ms) {
          for (entry in names(out)) {
            if (is.list(out[[entry]])) {
              cur_val <- list(cur_ms[[entry]])
            } else {
              cur_val <- cur_ms[[entry]]
            }
            if (!is.null(cur_ms[[entry]])) {
              out[[entry]] <- c(out[[entry]], cur_val)
            } else {
              out[[entry]] <- c(out[[entry]], list(NULL))
            }
          }
        }
      }
    }
    
    if (length(out$id)==1) {
      for (entry in names(out)) {
        out[[entry]] <- list(out[[entry]])
      }
    }
    out
  },
  rm_measurements=function(obj_or_id) {
    "remove registered measurments from a given data object"
    ms_obj <- NULL
    if (is.character(obj_or_id)) {
      # passed the id instead of the object
      id <- obj_or_id
      if (!exists(id, envir=.self$.ms_list, inherits=FALSE)) {
        stop("measurement with id ", id, " not found")
      }
      ms_obj <- .self$.ms_list[[id]]$obj
    } else {
      ms_obj <- obj_or_id
    }
    
    if (!is(ms_obj, "EpivizData")) {
      stop("'ms_obj' must be an 'EpivizData' object")
    }
    
    id <- ms_obj$get_id()
    if (!exists(id, envir=.self$.ms_list, inherits=FALSE)) {
      stop("measurement with id ", id, " not found")
    }
    
    ms_record <- .self$.ms_list[[id]]
    ms_name <- ms_record$name
    ms <- ms_record$obj$get_measurements()
    rm(list=id, envir=.self$.ms_list)
    
    if(isTRUE(ms_record$connected)) {
      callback <- function(response_data) {
        cat("measurement object ", ms_name, " removed and disconnected\n")
      }
      request_data <- list(action="removeMeasurements",
                           measurements=epivizrServer::json_writer(ms))
      .self$.server$send_request(request_data, callback)
    }
    invisible()
  },
  rm_allMeasurements = function() {
    "remove all registered measurements"
    ids <- ls(.self$.ms_list)
    if (length(ids)>0) {
      for (id in ids) {
        .self$rm_measurements(id)
      }
    }
  },
  list_measurements = function() {
    "make a printable list of registered measurements"
    if (.self$num_datasources() == 0) {
      return(data.frame())
    }
    
    ids <- ls(.self$.ms_list)
    nms <- sapply(ids, function(id) .self$.ms_list[[id]]$name)
    lens <- sapply(ids, function(id) length(.self$.ms_list[[id]]$obj$.object))
    connected <- ifelse(sapply(ids, function(id) .self$.ms_list[[id]]$connected), "*", "")
    columns <- sapply(ids, function(id) paste0(.self$.ms_list[[id]]$obj$.columns, collapse=","))
    
    data.frame(id=ids,
               name=nms,
               length=lens,
               connected=connected,
               columns=columns,
               stringsAsFactors=FALSE, row.names=NULL)
  }
)
