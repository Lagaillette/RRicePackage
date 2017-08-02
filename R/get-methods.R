#' @rdname getName-methods
#' @aliases getName,Experiment-method
#' @examples 
#' exp <- new(Class="Experiment", date=Sys.Date(), name="test")
#' getName(exp)
setMethod(
    "getName",
    signature = "Experiment",
    def = function(object){
        return(object@name)
    }
)