EpivizMeasurement <- setClass("EpivizMeasurement",
  slots = c(
    id = "character",
    name = "character",
    type = "character",
    datasourceId = "character",
    datasourceGroup = "character",
    defaultChartType = "character",
    annotation = "ANY",
    minValue = "numeric",
    maxValue = "numeric",
    metadata = "ANY"
  ),
  prototype = prototype(
    id = character(),
    name = character(),
    type = character(),
    datasourceId = character(),
    datasourceGroup = character(),
    defaultChartType = character(),
    annotation = NULL,
    minValue = as.numeric(NA),
    maxValue = as.numeric(NA),
    metadata = NULL
  )
)

setMethod("as.list", signature(x="EpivizMeasurement"),
  function(x) {
    nms <- slotNames("EpivizMeasurement")
    out <- lapply(nms, function(slot_name) slot(x, slot_name))
    names(out) <- nms
    out
  }          
)
