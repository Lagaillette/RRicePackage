#' Set the name of an Experiment
#'
#' @param object The Experiment for which we want to set the name
#' @param name The name we want to give at the object
#' @return The object with the new name
setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)