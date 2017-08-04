#' @rdname getAttributesNames-methods
#' @aliases getAttributesNames,Experiment-method
setMethod(
    "getAttributesNames",
    signature = "Experiment",
    def = function(object){
        res <- attributes(object)
        names(res)
    }
)
    
#' @rdname getAttributesNames-methods
#' @aliases getAttributesNames,Gene-method
setMethod(
    "getAttributesNames",
    signature = "Gene",
    def = function(object){
        res <- attributes(object)
        names(res)
    }
)