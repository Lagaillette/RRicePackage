#' @rdname deleteAttribute-methods
#' @aliases deleteAttribute,Experiment-method
#' @examples
#' exp <- new(Class="Experiment",
#' name="test",
#' date=Sys.Date(),
#' databases=list(1),
#' others=list())
#' exp <- addAttribute(exp, "test", "value")
#' exp <- deleteAttribute(exp, "test")
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
#' @examples 
#' gene <- new("RAPDB")
#' gene <- addAttribute(gene, "test", "value")
#' gene <- deleteAttribute(gene, "test")
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