EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  methods=list(
  	initialize=function(...) {
  		callSuper(...)
  		.self$.columns <- NULL
  	}#,
    # plot=function(...) {
    #   mgr$blockChart(ms=getMeasurements(), ...)
    # }
  )
)

.valid.EpivizBlockData.ylim <- function(x) {
	if (!is.null(x$.ylim))
		return("'ylim' must be 'NULL'")
	NULL
}

.valid.EpivizBlockData <- function(x) {
	c(.valid.EpivizBlockData.ylim(x))
}

S4Vectors::setValidity2("EpivizBlockData", .valid.EpivizBlockData)

EpivizBlockData$methods(
  get_measurements = function() {
    out <- list(list(id = .self$.id,
                name = .self$.name,
                type = "range",
                datasourceId = .self$.id,
                datasourceGroup = .self$.id,
                defaultChartType="Blocks Track",
                annotation=NULL,
                minValue=NA,
                maxValue=NA,
                metadata=NULL))
      out
  })#,
#  parseMeasurement=function(msId) {
#     if (msId != id)
#       stop("invalid parsed measurement")
#     NULL
#   },
#   .getMetadata=function(curHits, metadata) {
#     return(NULL)
#   },
#   .getValues=function(curHits, measurement) {
#     return(NULL)
#   }
# )
# 
