#' @rdname getName-methods
#' @aliases getName,Experiment-method
#' @examples 
#' exp <- new(Class="Experiment", date=Sys.Date(), name="test")
#' getName(exp)
setMethod(
    "getName",
    signature = "Experiment",
    def = function(object){
        return(object@name)
    }
)

#' @rdname getDate-methods
#' @aliases getDate,Experiment-method
#' @examples 
#' exp <- new(Class="Experiment",
#'  date=Sys.Date(),
#'   name="ex",
#'    databases=list(1))
#' getDate(exp)
setMethod(
    "getDate",
    signature = "Experiment",
    def = function(object){
        return(object@date)
    }
)

#' @rdname getId-methods
#' @aliases getId,Gene-method
#' @examples 
#' gene <- new("Gene", id="exId" )
#' 
#' getId(gene)
setMethod(
    "getId",
    signature = "Gene",
    def = function(object){
        return(object@id)
    }
)

#' @rdname getLocus-methods
#' @aliases getLocus,Gene-method
#' @examples 
#' gene <- new("Gene", id="exId", locus=data.frame(1,222,333) )
#' 
#' getLocus(gene)
setMethod(
    "getLocus",
    signature = "Gene",
    def = function(object){
        return(object@locus)
    }
)

#' @rdname getGenes-methods
#' @aliases getGenes,Experiment-method
#' @examples 
#' gene <- new("Gene", id="exId", locus=data.frame(1,222,333) )
#' 
#' exp <- new(Class="Experiment",
#'  date=Sys.Date(),
#'   name="ex",
#'   genes=list(list(gene)),
#'    databases=list(1))
#' 
#' getGenes(exp)
setMethod(
    "getGenes",
    signature = "Experiment",
    def = function(object){
        return(object@genes)
    }
)