#' Class providing data manager for epiviz app
#'
#' @docType class
#' @import methods
#' @importClassesFrom epivizrServer EpivizServer
#' 
#' @details
#' The 'register_type' method is used to register data types to be handled by the
#' manager. Usage is as follows:
#' \code{register_type(type_name, type_descriptor)}: with \code{type_name} a character
#' string specifying the type name and \code{type_descriptor} a list with slots
#' \code{class} specifying with \code{EpivizData} subclass it corresponds to,
#' \code{description} describing the data type and 
#' \code{input_class} specifying which object type is taken as input
#' 
EpivizDataMgr <- setRefClass("EpivizDataMgr",
  fields = list(
    .ms_list = "list",
    .type_map = "list",
    .ms_idCounter = "integer",
    .server = "EpivizServer"
  ),
  methods=list(
    initialize=function(server, ...) {
      .self$.server <- server
      .self$.ms_list <- vector("list")
      .self$.type_map <- vector("list")
      .self$.ms_idCounter <- 0L
    },
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
    is_server_closed = function() {
      "Check if underlying server is closed, <logical>"
      .server$is_closed()
    },
    register_type=function(type_name, type_descriptor) {
      "Register a data type to the manager (See Details)"
      if (is.null(type_descriptor$class) ||
          is.null(type_descriptor$description) ||
          is.null(type_descriptor$input_class))
        stop("type_descriptor is not valid see ?`EpivizDataMgr-class`")
      
      .self$.type_map[[type_name]] <- type_descriptor
      .self$.ms_list[[type_name]] <- new.env()
    }
  )
)

