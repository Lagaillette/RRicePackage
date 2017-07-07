#' @return The object with the new name
#' @rdname setName-methods
#' @aliases setName,Experiment-method
setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)