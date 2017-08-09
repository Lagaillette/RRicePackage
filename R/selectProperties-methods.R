#' @rdname selectProperties-methods
#' @aliases selectProperties,Experiment-method
setMethod(
    "selectProperties",
    signature = "Experiment",
    definition = function(object, attributesVector){
        object@properties <- append(object@properties, attributesVector)
        return(object)
    }
)