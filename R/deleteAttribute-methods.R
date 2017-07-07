#' @export
#' @aliases deleteAttribute,Experiment-method
#' @rdname Experiment-class
setMethod(
    "deleteAttribute",
    signature = "Experiment",
    def = function(object, name){
        i <- 1
        find <- FALSE
        while(!find && i<=length(object@others)){
            if(names(object@others[[i]]) == name){
                print("I am in")
                object@others <- object@others[-i]
                find <- TRUE
            }
            i <- i+1
        }
        return(object)
    }
)

setMethod(
    "deleteAttribute",
    signature = "Gene",
    def = function(object, name){
        i <- 1
        find <- FALSE
        while(!find && i<=length(object@others)){
            if(names(object@others[[i]]) == name){
                print("I am in")
                object@others <- object@others[-i]
                find <- TRUE
            }
            i <- i+1
        }
        return(object)
    }
)