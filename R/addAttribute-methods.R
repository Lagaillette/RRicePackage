#' @name addAttribute
#' @rdname addAttribute-methods
#' @aliases addAttribute,Experiment-method
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