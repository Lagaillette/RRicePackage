#' @return The object with the new name
#' @rdname setName-methods
#' @aliases setName,Experiment-method
#' exp <- new(Class="Experiment", date=Sys.Date(), name="test")
#' setName(exp, "name")
setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)