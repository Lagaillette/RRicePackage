#' Constructor of an Experiment
#' 
#' allows to create an experiment 
#' 
#' @param name the name of the experiment
#' @param date the date of the experiment
#' @param databases the list of the databases
#' @param genes the list of genes of the databases
Experiment <- function(name, date, databases,genes){
    result <- new("Experiment",
                  name=name,
                  date=date,
                  databases=databases,
                  genes=genes,
                  others=list())
    return(result)
}


#' Constructor of a GeneDB1
#' 
#' This constructor allows to create an Experiment from the RAPDB Database.
#'
#' @param id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @param locus The locus the gene belongs.
#' @param rapDBGeneNameSynonym an identificator
#' @param rapDBGeneSymbolSynonym an identificator
#' @param cgsnlGeneName an identificator
#' @param cgsnlGeneSymbol an identificator
#' @param oryzabaseGeneNameSynonym an identificator
#' @param oryzabaseGeneSymbolSynonym an identificator
#' @param position position of the gene into the chromosome
#' @param description a description of the gene
#' @return An Experiment
#' @import methods
#' @import jsonlite
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


#' Constructor of a GeneDB3
#' 
#' This constructor allows to create a GeneDB3 from the DB oryzabase.
#'
#' @param id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @param locus The locus the gene belongs.
#' @param traitGeneId id
#' @param cgsnlGeneSymbol id
#' @param GeneSymbolSynonim id
#' @param cgsnlSymbolSynonim id
#' @param GeneNameSynonim id
#' @param proteinName id
#' @param allele id
#' @param chromosomeNumber id
#' @param explanation id
#' @param traitClass id
#' @param rapID id
#' @param grameneId id
#' @param arm id
#' @param locate id
#' @param geneOntology id
#' @param traitOntology id
#' @param plantOntology id
#' @return An Experiment
#' @import methods
#' @rdname GeneDB3-class
GeneDB3 <- function(id, locus,traitGeneId, cgsnlGeneSymbol, GeneSymbolSynonim,
                    cgsnlSymbolSynonim, GeneNameSynonim, proteinName,
                    allele, chromosomeNumber, explanation, traitClass,
                    rapID, grameneId, arm, locate, geneOntology,
                    traitOntology, plantOntology ){
    
    result <- new("GeneDB3",
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
