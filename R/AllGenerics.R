###############################################################################
############################### setGeneric ####################################


#' Update an attribute of an object
#'
#' @param object The object for which we want modification
#' @param attribute The attribute we want to update
#' @param value The value we want to give at the attribute
#' @return The object without the attribute we want to delete
#' @exportMethod updateObject
#' @name updateObject
#' @rdname updateObject-methods
#' @docType methods
setGeneric(
    name = "updateObject",
    def = function(object, attribute , value){
          standardGeneric("updateObject" )}
)


#' Add an attribute to an object.
#'
#' @param object The object for which we want modification.
#' @param name The name of the attribute we want to add.
#' @param value The value we want to give to the attribute.
#' @return The object without the attribute you want to delete.
#' @exportMethod addAttribute
#' @name addAttribute
#' @rdname addAttribute-methods
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
#' @rdname deleteAttribute-methods
#' @exportMethod deleteAttribute
#' @docType methods
setGeneric(
    name = "deleteAttribute",
    def = function(object, name){standardGeneric("deleteAttribute" )}
)

#' Get all the genes of the data base you want.
#'
#' @param object The object for which we want modification.
#' @param nbDB The number of the db you want(ex : 1 for RAPDB).
#' @return the list of the genes of the database you want.
#' @exportMethod getDBGenes
#' @name getDBGenes
#' @rdname getDBGenes-methods
#' @exportMethod getDBGenes
#' @docType methods
setGeneric(
    name ="getDBGenes",
    def = function(object, nbDB){standardGeneric("getDBGenes" )}
)

#' Get the attribute names  of an object.
#'
#' @param object The object for which we want to know the attribute names.
#' @return the list of the attributes wanted.
#' @exportMethod getAttributesNames
#' @name getAttributesNames
#' @rdname getAttributesNames-methods
#' @exportMethod getAttributesNames
#' @docType methods
setGeneric(
    name ="getAttributesNames",
    def = function(object){standardGeneric("getAttributesNames" )}
)


#' get the properties you want to keep
#'
#' @param object The object for which we want to extract a dataframe
#' @param idWanted id you want to keep to identify your gene
#' @return a data frame with the genes and the properties you want 
#' @exportMethod getProperties
#' @name getProperties
#' @rdname getProperties-methods
#' @docType methods
setGeneric(
    name = "getProperties",
    def = function(object, idWanted){standardGeneric("getProperties" )}
)

#' select the properties you want to study
#'
#' @param object The object for which we want to extract a dataframe
#' @param attributesVector list of the attributes you want to keep
#' @return a data frame with the genes and the properties you want 
#' @exportMethod selectProperties
#' @name selectProperties
#' @rdname selectProperties-methods
#' @docType methods
setGeneric(
    name = "selectProperties",
    def = function(object, attributesVector){standardGeneric("selectProperties" )}
)

######################### Getters & setters ###################################

#' Get the name of an object
#'
#' @param object The object for which we want to get the name
#' @return The name of the object
#' @exportMethod getName
#' @name getName
#' @rdname getName-methods
#' @docType methods
setGeneric(
    name = "getName",
    def = function(object){standardGeneric("getName" )}
)




#' Set the name of an object
#'
#' @param object The object for which we want to set the name
#' @param name The name we want to give at the object
#' @return The object with the new name
#' @exportMethod setName
#' @name setName
#' @rdname setName-methods
#' @docType methods
setGeneric(
    name = "setName",
    def = function(object, name){standardGeneric("setName" )}
)

#' Get the date of an object
#'
#' @param object The object for which we want to get the name
#' @return The date of the object
#' @exportMethod getDate
#' @name getDate
#' @rdname getDate-methods
#' @docType methods
setGeneric(
    name = "getDate",
    def = function(object){standardGeneric("getDate" )}
)


#' Set the date of an object
#'
#' @param object The object for which we want to set the name
#' @param date The date we want to give at the object
#' @return The object with the new name
#' @exportMethod setDate
#' @name setDate
#' @rdname setDate-methods
#' @docType methods
setGeneric(
    name = "setDate",
    def = function(object, date){standardGeneric("setDate" )}
)

#' Get the id of an object
#'
#' @param object The object for which we want to get the id
#' @return The id of the object
#' @exportMethod getId
#' @name getId
#' @rdname getId-methods
#' @docType methods
setGeneric(
    name = "getId",
    def = function(object){standardGeneric("getId" )}
)

#' Get the locus of an object
#'
#' @param object The object for which we want to get the locus
#' @return The locus of the object
#' @exportMethod getLocus
#' @name getLocus
#' @rdname getLocus-methods
#' @docType methods
setGeneric(
    name = "getLocus",
    def = function(object){standardGeneric("getLocus" )}
)

#' Get the genes of an object
#'
#' @param object The object for which we want to get the genes
#' @return The genes of the object
#' @exportMethod getGenes
#' @name getGenes
#' @rdname getGenes-methods
#' @docType methods
setGeneric(
    name = "getGenes",
    def = function(object){standardGeneric("getGenes" )}
)



