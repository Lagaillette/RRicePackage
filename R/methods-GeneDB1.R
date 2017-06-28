###############################################################################
################################ Constructor ##################################


GeneDB1 <- function(id, locus, rapDBGeneNameSynonym, rapDBGeneSymbolSynonym,
                    cgsnlGeneName, cgsnlGeneSymbol, oryzabaseGeneNameSynonym,
                    oryzabaseGeneSymbolSynonym, description){
    
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
                  description = description)
    
    return(result)
          
}


###############################################################################
################################# Methods #####################################
