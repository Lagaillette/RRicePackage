#' Get the name of an Experiment
#'
#' @param object The Experiment for which we want to get the name
#' @return The name of the object
#' @exportMethod getName
setMethod(
    "getName",
    signature = "Experiment",
    def = function(object){
        return(object@name)
    }
)