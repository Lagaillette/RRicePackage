#' @aliases updateObject,Experiment-method
#' @rdname updateObject-methods
#' @aliases updateObject,Experiment-method
setMethod(
    "updateObject",
    signature = "Experiment",
    def = function(object, attribute, value){
        name_setter = paste("set",
                            toupper(substr(attribute, 1, 1)),
                            substr(attribute, 2, nchar(attribute)),
                            sep = "")
        experiment<- do.call(name_setter, list(experiment, value))
        return(experiment)
    }
)