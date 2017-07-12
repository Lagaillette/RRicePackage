library(jsonlite)

##on relie au fichier methods-GeneDB1.R to call function GeneDB1
##A faire avec Roxygen2 au dessus de classes @export
##source("rRice/R/constructors-functions.R")
##source("rRice//R/AllClasses.R")

command ="python3"

#'Function for a JSON return
#'
#' this function will only return an JSON return which start with "{"
#' It will allow us to treat the exception error from python 
#' 
#' @return return only string which starts with "{" -> JSON 
#' @rdname getOutPutJSON-function
getOutPutJSON <- function (outPut) {
    if (identical(substr(outPut,0,1),'{')) {
        return(outPut)
    }
}

#' creationGeneDB1
#'
#' This function is called only by callDB1 and will create the gene DB1
#' It will call run.py script which will return the list of the genes which 
#' are present in the locus
#' 
#' @param integer i,locusList list
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname creationGeneDB1-function
creationGeneDB1 <- function (i, locusList) {
    
    ##PATH for package when it will be installed -> when it will be released
    path <- paste(system.file(package = "rRice"), "Python/rricebeta/rricebeta/run.py", sep="/")
    path2Script = paste(c(path), collapse = '')
    
    ##While I am using rRice, I use that
    # debut = getwd()
    # path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    # path2Script = paste(c(debut,path), collapse = '')
    
    ch = as.character(locusList[i,1])
    start = as.character(locusList[i,2])
    end = as.character(locusList[i,3])
    
    ##appel du script python run.py avec les attributs (chx, start, end, DB) 
    ##-> tous les attributs doivent etre en chaine de carac
    args = c(ch, start, end, "1")
    allArgs = c(path2Script, args)
    rOutput = system2(command, args=allArgs, stdout=TRUE)
    ##print(rOutput)
    
    rOutput <- lapply(1 : length(rOutput), 
                      function(x) getOutPutJSON(rOutput[x]))
    
    rOutput[sapply(rOutput, is.null)] <- NULL
            
    ##if rOutput is an empty list then we don't create a new GeneDB1
    if (length(rOutput) > 0) {
        jsonOutput <- fromJSON(rOutput[[1]])
        
        idRec = jsonOutput["ID"]
        position = jsonOutput["Position"]
        rapSymbol = jsonOutput["RAP-DB Gene Symbol Synonym(s)"]
        cgsnlName = jsonOutput["CGSNL Gene Name"]
        oryGeneSymbol = jsonOutput["Oryzabase Gene Symbol Synonym(s)"]
        description = jsonOutput["Description"]
        rapName = jsonOutput["RAP-DB Gene Name Synonym(s)"]
        oryGeneName = jsonOutput["Oryzabase Gene Name Synonym(s)"]
        cgsnlGene = jsonOutput["CGSNL Gene Symbol"]
 
        position <- as.character(position) 
        pos1 <- strsplit(position, ":")
        pos2 <- pos1[[1]][[2]]
        pos3 <- strsplit(pos2,"[..]")
        
        positionData <- data.frame(ch=c(pos1[[1]][[1]]),
                                   st=c(pos3[[1]][[1]]),
                                   end=c(pos3[[1]][[3]]))
        
        dataLocus <- data.frame(ch = ch, st = start, end = end)
        
        newGene <- GeneDB1(as.character(idRec),
                           dataLocus,
                           as.character(rapName),
                           as.character(rapSymbol),
                           as.character(cgsnlName),
                           as.character(cgsnlGene),
                           as.character(oryGeneName),
                           as.character(oryGeneSymbol),
                           positionData,
                           as.character(description))

        return(newGene)
    }
}

#' callDB1
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB1. All these locus will be stocked in listGenes
#' 
#' @param locusList list of locus for which we want the genes
#' @export
#' @rdname callDB1-function
callDB1 <- function (locusList) {
    
    listGenes <- data.frame()
    
    ##We call the function creationGeneDB1 to create our newGene
    listGenes <- lapply(1 : nrow(locusList),
         FUN = function(x) creationGeneDB1(x, locusList))

    
    ##Remove all the NULL object from the list
    listGenes[sapply(listGenes, is.null)] <- NULL
    
    ##To delete all the geneDB1 which exists in double
    listGenes <- unique(listGenes)

    ##print(listGenes)
    return (listGenes)
}

######################

#' creationGeneDB3
#'
#' This function is called only by callDB3 and will create the gene DB3
#' It will call run.py script which will return the list of the genes which 
#' are present in the locus
#' 
#' @param integer i,locusList list
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname creationGeneDB3-function
creationGeneDB3 <- function (i, locusList) {
    ##PATH for package when it will be installed -> when it will be released
    path <- paste(system.file(package = "rRice"), "Python/rricebeta/rricebeta/run.py", sep="/")
    path2Script = paste(c(path), collapse = '')
    
    ##While I am using rRice, I use that
    # debut = getwd()
    # path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    # path2Script = paste(c(debut,path), collapse = '')
    
    ch = as.character(locusList[i,1])
    start = as.character(locusList[i,2])
    end = as.character(locusList[i,3])
    
    ##appel du script python run.py avec les attributs (chx, start, end, DB) 
    ##-> tous les attributs doivent etre en chaine de carac
    args = c(ch, start, end, "3")
    allArgs = c(path2Script, args)
    rOutput = system2(command, args=allArgs, stdout=TRUE)
    ##print(rOutput)
    
    rOutput <- lapply(1 : length(rOutput), 
                      function(x) getOutPutJSON(rOutput[x]))

    rOutput[sapply(rOutput, is.null)] <- NULL

    ##if rOutput is an empty list then we don't create a new GeneDB1
    if (length(rOutput) > 0) {
        jsonOutput <- fromJSON(rOutput[[1]])

        traitGeneId = jsonOutput["Trait Gene Id"]
        cgsnlGeneSymbol =jsonOutput["CGSNL Gene Symbol"]
        geneSymbolSynonym =jsonOutput["Gene symbol synonym(s)"]
        cgsnlGeneName = jsonOutput["CGSNL Gene Name"]
        geneNameSynonym =jsonOutput["Gene name synonym(s)"]
        proteinName =jsonOutput["Protein Name"]
        allele = jsonOutput["Allele"]
        chromosomeNo = jsonOutput["Chromosome No."]
        explanation =jsonOutput["Explanation"]
        traitClass =jsonOutput["Trait Class"]
        rapId =jsonOutput["RAP ID"]
        grameneId =jsonOutput["Gramene ID"]
        arm =jsonOutput["Arm"]
        locateCm = jsonOutput["Locate(cM)"]
        geneOntology = jsonOutput["Gene Ontology"]
        traitOntology = jsonOutput["Trait Ontology"]
        plantOntology = jsonOutput["Plant Ontology"]

        if (!existsGene(listGenes,as.character(traitGeneId))) {
            newGene <- GeneDB3("",
                               locusList[i,],
                               as.character(traitGeneId),
                               as.character(cgsnlGeneSymbol),
                               as.character(geneSymbolSynonym),
                               as.character(cgsnlGeneName),
                               as.character(geneNameSynonym),
                               as.character(proteinName),
                               as.character(allele),
                               as.character(chromosomeNo),
                               as.character(explanation),
                               as.character(traitClass),
                               as.character(rapId),
                               as.character(grameneId),
                               as.character(arm),
                               as.character(locateCm),
                               as.character(geneOntology),
                               as.character(traitOntology),
                               as.character(plantOntology))
        }

        return(newGene)
    }
}

#' callDB3
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB3. All these locus will be stocked in listGenes
#' 
#' @param locusList list
#' @export
#' @rdname callDB3-function
callDB3 <- function (locusList) {
    
    listGenes <- data.frame()
    
    ##We call the function creationGeneDB1 to create our newGene
    listGenes <- lapply(1 : nrow(locusList),
                 FUN = function(x) creationGeneDB3(x, locusList))
    
    
    ##Remove all the NULL object from the list
    listGenes[sapply(listGenes, is.null)] <- NULL
    
    ##To delete all the geneDB1 which exists in double
    listGenes <- unique(listGenes)

    ##print(listGenes)
    return (listGenes)
}

############################

##Test phases
# data <- data.frame(ch = c("1","1","1","1","1"),
#                   st = c("148907","5671734","9344261","10225320","148907"),
#                   end = c("248907","6337629","11332201","10325320","248907"))

#data <- data.frame(ch = c("1","1","1"),
#                   st = c("148907","9344261","148907"),
#                   end = c("248907","11332201","248907"))

 # data <- data.frame(ch = c("1"),
 #                    st = c("148907"),
 #                    end = c("248907"))

# data <- data.frame(ch = c("1"),
#                    st = c("9344261"),
#                    end = c("11332201"))

#print(data)
#s <- callDB3(data)
#print(s)