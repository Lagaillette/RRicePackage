#This file contains the functions which not concern an existing class
#Contains also all the "setGeneric" methods


###############################################################################
############################### setGeneric ####################################

setGeneric(
    name = "searchGeneProperty",
    def = function(object, search){standardGeneric("searchGeneProperty" )}
)

setGeneric(
    name = "searchGeneFmin",
    def = function(object, search){standardGeneric("searchGeneFmin" )}
)


#' Update an attribute of an object
#'
#' @param object The object for which we want modification
#' @param attribute The attribute we want to update
#' @param value The value we want to give at the attribute
#' @return The object without the attribute we want to delete
#' @exportMethod updateObject
#' @name updateObject
#' @rdname updateObject-method
#' @docType methods
setGeneric(
    name = "updateObject",
    def = function(experiment, attribute , value){standardGeneric("updateObject" )}
)


#' Get the name of an object
#'
#' @param object The object for which we want to get the name
#' @return The name of the object
#' @exportMethod getName
setGeneric(
    name = "getName",
    def = function(object){standardGeneric("getName" )}
)


#' Set the name of an object
#'
#' @param object The object for which we want to set the name
#' @param name The name we want to give at the object
#' @return The object with the new name
setGeneric(
    name = "setName",
    def = function(object, name){standardGeneric("setName" )}
)

#' Add an attribute to an object.
#'
#' @param object The object for which we want modification.
#' @param name The name of the attribute we want to add.
#' @param value The value we want to give to the attribute.
#' @return The object without the attribute you want to delete.
#' @exportMethod addAttribute
#' @name addAttribute
#' @rdname addAttribute-method
#' @docType methods
setGeneric(
    name = "addAttribute",
    def = function(object, name, value){standardGeneric("addAttribute" )}
)

#' Delete an attribute which has been add by the user.
#'
#' @param object The object for which we want modification.
#' @param name The name of the attribute we want to delete.
#' @return The object without the attribute you want to delete.
#' @exportMethod deleteAttribute
#' @name deleteAttribute
#' @rdname deleteAttribute-method
#' @docType methods
setGeneric(
    name = "deleteAttribute",
    def = function(object, name){standardGeneric("deleteAttribute" )}
)

###############################################################################
################################ functions ####################################


# callDB functions --------------------------------------------------

callDB1 <- function(database){
    if(database == 1){
        csv <- read.csv("/home/ioit.user3/RRicePackage/R/beginning/test/outputRAPDB.csv", stringsAsFactors = FALSE)
        genes <- data.frame(length=nrow(csv))
        for ( i in 1:nrow(csv) ) {
            genes[i] <- new ("GeneDB1", uniquename = csv[[1]][[i]], locus = data.frame(), msU7name = csv[[9]][[i]], fgeneshName = csv[[4]][[i]], rappredname = csv[[5]][[i]], fmin = as.numeric(csv[[6]][[i]]), fmax = as.numeric(csv[[2]][[i]]),contig = csv[[7]][[i]],iricname = csv[[8]][[i]], strand = as.numeric(csv[[10]][[i]]), description = as.character(csv[[11]][[i]], others = list()))
        }
    return(genes)
    }
}



