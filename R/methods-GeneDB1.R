###############################################################################
################################ Constructor ##################################

#' Constructor of a GeneDB1
#' 
#' This constructor allows to create an Experiment from the RAPDB Database.
#'
#' @param id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @param locus The locus the gene belongs.
#' @param others The others attributes the user want to add or delete.
#' @param rapDBGeneNameSynonym an identificator
#' @param rapDBGeneSymbolSynonym an identificator
#' @param cgsnlGeneName an identificator
#' @param cgsnlGeneSymbol an identificator
#' @param oryzabaseGeneNameSynonym an identificator
#' @param oryzabaseGeneSymbolSynonym an identificator
#' @param description a description of the gene
#' @return An Experiment
#' @import methods
#' @rdname GeneDB1-class
GeneDB1 <- function(id, locus, rapDBGeneNameSynonym, rapDBGeneSymbolSynonym,
                    cgsnlGeneName, cgsnlGeneSymbol, oryzabaseGeneNameSynonym,
                    oryzabaseGeneSymbolSynonym, position, description){
    
    result <- new("GeneDB1",
                  id = id,
                  locus = locus,
                  others = list(),
                  rapDBGeneNameSynonym = rapDBGeneNameSynonym,
                  rapDBGeneSymbolSynonym = rapDBGeneSymbolSynonym,
                  cgsnlGeneName = cgsnlGeneName,
                  cgsnlGeneSymbol = cgsnlGeneSymbol,
                  oryzabaseGeneNameSynonym = oryzabaseGeneNameSynonym,
                  oryzabaseGeneSymbolSynonym = oryzabaseGeneSymbolSynonym,
                  position = position,
                  description = description)
    
    return(result)
    
}


###############################################################################
################################# Methods #####################################
