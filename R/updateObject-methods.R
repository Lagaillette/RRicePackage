#' @export
#' @param object the Experiment we want to update
#' @param attribute the attribute we want to update
#' @param value the value we want to give to the attribute
#' @aliases updateObject,Experiment-method
#' @rdname Experiment-class
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