#' Class providing data manager for epiviz app
#'
#' @docType class
#' @import methods
#' @importClassesFrom epivizrServer EpivizServer
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
    register_type=function(type_name, type_description) {
      "Register a data type to the manager, type_name<character>, type_description<character>"
      .self$.type_map[[name]] <- type_descriptor
      .self$.ms_list[[name]] <- new.env()
    }
  )
)
# 
# 
