#' @return The object with the new name
#' @rdname setName-methods
#' @aliases setName,Experiment-method
#' @examples
#' exp <- new(Class="Experiment",
#'  databases=list(1),
#'  date=Sys.Date(),
#'  name="test")
#' exp <- setName(exp, "testName")
setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)