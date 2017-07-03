###############################################################################
################################ Constructor ##################################


GeneDB1 <- function(id, locus,traitGeneId, cgsnlGeneSymbol, GeneSymbolSynonim,
                    cgsnlSymbolSynonim, GeneNameSynonim, proteinName,
                    allele, chromosomeNumber, explanation, traitClass,
                    rapID, grameneId, arm, locate, geneOntology,
                    traitOntology, plantOntology ){
    
    result <- new("GeneDB1",
                  id = id,
                  locus = locus,
                  others = list(),
                  traitGeneId=traitGeneId,
                  cgsnlGeneSymbol=cgsnlGeneSymbol,
                  GeneSymbolSynonim=GeneSymbolSynonim,
                  cgsnlSymbolSynonim=cgsnlSymbolSynonim,
                  GeneNameSynonim=GeneNameSynonim,
                  proteinName=proteinName,
                  allele=allele,
                  chromosomeNumber=chromosomeNumber,
                  explanation=explanation,
                  traitClass=traitClass,
                  rapID=rapID,
                  grameneId=grameneId,
                  arm=arm,
                  locate=locate,
                  geneOntology=geneOntology,
                  traitOntology=traitOntology,
                  plantOntology=plantOntology)
    
    return(result)
    
}


###############################################################################
################################# Methods #####################################
