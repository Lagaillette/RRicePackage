#' Constructor of an Experiment
#' 
#' This constructor allows to create an Experiment object, with a name and a
#' locus list. The locus list is a .txt file, so you have to do a "read.table"
#' of your file into a variable and put this variable as the "locus" parameter
#' in the constructor. 
#'
#' @param name The name of the Experiment
#' @param locus The Table of locus 
#' @return An Experiment
#' @export
#' @aliases Experiment,Experiment-method
#' @rdname Experiment-class
Experiment <- function(name, locus){
    ## the number of databases available. To increment every time we have
    ## one more database available
    dbAvailables <- 3
    ## to check if the number the user will put is correct or not
    correctNbdb <- FALSE
    while(!correctNbdb){
        nbdb <- as.numeric(
            readline(
                prompt="How many databases do you want to experiment : "))
        if(is.numeric(nbdb) && nbdb > 0 && nbdb <= dbAvailables){
            databases <- vector(mode='list', length=nbdb)
            correctNbdb <- TRUE
        }
        else{
            message("you have not choose a correct number of databases.")
            message("please try again")
        }
    }
    ## Allows to check if the date is good or not
    correctDate <- FALSE
    while(!correctDate){
        date <- (readline(
            prompt="Enter the date of the experiment (mm/dd/yyyy) : "))
        date <- as.Date(c(date), format =  "%m/%d/%Y")
        if(!is.na(date) && 
           format(date, '%Y') > 1900 &&
           format(date, '%Y') <= format(Sys.Date(), '%Y')){
            correctDate <- TRUE
        }
        else{
            message("please write the date in the format asked.")
        }
    }
    ## We create the list which will have the genes of the databases
    genes <- vector(mode='list', length=nbdb)
    i <- 1
    while(i <= nbdb){
        databases[i] <- as.numeric(
            readline(
                prompt = paste("Enter the number",
                               "of the database you want",
                               "(example : 1 for RAPDB) :")))
        if(databases[i] > 0 && 
           databases[i] <= dbAvailables && 
           !alreadyUsedDB(databases,i)){
            callDB <- paste("callDB",databases[i],sep="")
            ##genes[[i]] <- (do.call(callDB, args = list(locus)))
            i <- i+1
        }
        else{
            if(alreadyUsedDB(databases,i)){
                message("the database is already used. Try again")
            }
            else{
                message("this number of database not exists. Try again")
                message("You can put a number between 1 and 3")
            }
            
        }
    }
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
