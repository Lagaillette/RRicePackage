#' @rdname getName-methods
#' @aliases getName,Experiment-method
setMethod(
    "getName",
    signature = "Experiment",
    def = function(object){
        return(object@name)
    }
)