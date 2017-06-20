#This file contains the functions which not concern an existing class
#Contains also all the "setGeneric" methods


#####################################################################
########################## setGeneric ###############################

setGeneric(name = "search_gene_property",
           def = function(object, search){standardGeneric("search_gene_property" )}
)

setGeneric(name = "search_gene_fmin",
           def = function(object, search){standardGeneric("search_gene_fmin" )}
)

setGeneric(name = "update_object",
           def = function(experiment, attribute , value){standardGeneric("update_object" )}
)

setGeneric(name = "get_name",
           def = function(object){standardGeneric("get_name" )}
)

setGeneric(name = "set_name",
           def = function(object, name){standardGeneric("set_name" )}
)

setGeneric(name = "add_attribute",
           def = function(object, name, value){standardGeneric("add_attribute" )}
)

setGeneric(name = "delete_attribute",
           def = function(object, name){standardGeneric("delete_attribute" )}
)

#####################################################################
########################### functions ###############################


# callDB functions --------------------------------------------------

callDB1 <- function(database){
  if(database == 1){
    csv <- read.csv("/home/ioit.user3/RRicePackage/R/beginning/test/outputRAPDB.csv")
    genes <- vector(mode='list', length=nrow(csv))
    for ( i in 1:nrow(csv) ) {
      genes[i] <- new ("Gene2", uniquename = csv[[1]][[i]], msU7name = csv[[9]][[i]], fgeneshName = csv[[4]][[i]], rappredname = csv[[5]][[i]], fmin = csv[[6]][[i]], fmax = csv[[2]][[i]],contig = csv[[7]][[i]],iricname = csv[[8]][[i]], strand = csv[[10]][[i]], description = as.character(csv[[11]][[i]]))
    }
    return(genes)
  }
}