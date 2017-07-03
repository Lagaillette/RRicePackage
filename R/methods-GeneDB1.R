###############################################################################
################################ Constructor ##################################


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
