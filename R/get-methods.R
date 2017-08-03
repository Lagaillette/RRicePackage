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

#' @rdname getDate-methods
#' @aliases getDate,Experiment-method
#' @examples 
#' exp <- new(Class="Experiment",
#'  date=Sys.Date(),
#'   name="ex",
#'    databases=list(1))
#' getDate(exp)
setMethod(
    "getDate",
    signature = "Experiment",
    def = function(object){
        return(object@date)
    }
)