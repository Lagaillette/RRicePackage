###############################################################################
################################### Class #####################################

#' The Gene class.
#'
#' A gene contains a lot of details that depend on the database it belongs. 
#' But every gens have information in common. So we put all these informations
#' into a global class "gene" from which the others specific gene class 
#' inherit.
#'
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot genes The list of genes we avec download.
#' @name Gene
#' @rdname Gene-class
#' @exportClass Gene
setClass(
  #name of the class
  "Gene",

  #attributes of the class
  slots = list(uniquename = "character", locus = "data.frame", others = "list")
)


#####################################################################
########################### Constructor #############################


Gene <- function(uniquename, locus){
  result <- new("Gene", uniquename = uniquename, locus = locus, others = list())
  return(result)
}


#####################################################################
############################ Methods ################################

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
