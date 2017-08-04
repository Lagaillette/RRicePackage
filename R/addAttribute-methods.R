#' @name addAttribute
#' @rdname addAttribute-methods
#' @aliases addAttribute,Experiment-method
#' @examples 
#' exp <- new(Class="Experiment",
#' name="test",
#' date=Sys.Date(),
#' databases=list(1),
#' others=list())
#' exp <- addAttribute(exp, "test", "value")
setMethod(
    "addAttribute",
    signature = "Experiment",
    def = function(object, name, value){
        object@others <- append(object@others, value)
        size <- length(object@others)
        print(size)
        print(length(object@others))
        names( object@others[[size]]) <- name
        return(object)
    }
)


#' @name addAttribute
#' @rdname addAttribute-methods
#' @aliases addAttribute,Gene-method
#' @examples 
#' gene <- new("RAPDB")
#' gene <- addAttribute(gene, "test", "value")
setMethod(
    "addAttribute",
    signature = "Gene",
    def = function(object, name, value){
        object@others <- append(object@others, value)
        size <- length(object@others)
        print(size)
        print(length(object@others))
        names( object@others[[size]]) <- name
        return(object)
    }
)