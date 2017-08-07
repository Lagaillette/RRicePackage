#' @rdname updateObject-methods
#' @aliases updateObject,Experiment-method
#' @examples 
#' gene1 <- new("RAPDB")
#' listGenes <- list(gene1)
#' exp <- new(Class="Experiment",
#' name="test",
#' date=Sys.Date(),
#' genes=listGenes,
#' databases=list(1),
#' others=list())
#' updateObject(exp,"name","test")
setMethod(
    "updateObject",
    signature = "Experiment",
    def = function(object, attribute, value){
        experiment <- object
        name_setter = paste("set",
                            toupper(substr(attribute, 1, 1)),
                            substr(attribute, 2, nchar(attribute)),
                            sep = "")
        experiment <- do.call(name_setter, list(experiment, value))
        return(experiment)
    }
)