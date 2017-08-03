#' @return The object with the new name
#' @rdname setName-methods
#' @aliases setName,Experiment-method
#' @examples
#' exp <- new(Class="Experiment",
#'  databases=list(1),
#'  date=Sys.Date(),
#'  name="test")
#'  
#' exp <- setName(exp, "example")
setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)

#' @return The object with the new date
#' @rdname setDate-methods
#' @aliases setDate,Experiment-method
#' @examples
#' exp <- new(Class="Experiment",
#'  databases=list(1),
#'  date=Sys.Date(),
#'  name="test")
#'  
#'  
#' exp <- setDate(exp, "12/18/2010")
setMethod(
    "setDate",
    signature = "Experiment",
    def = function(object,date){
        object@date <- as.Date(c(date), format =  "%m/%d/%Y")
        return(object)
    }
)
