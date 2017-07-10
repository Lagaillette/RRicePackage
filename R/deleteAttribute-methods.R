#' @rdname deleteAttribute-methods
#' @aliases deleteAttribute,Experiment-method
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


#' @name deleteAttribute
#' @rdname deleteAttribute-methods
#' @aliases deleteAttribute,Gene-method
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