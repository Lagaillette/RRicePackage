library(jsonlite)

##on relie au fichier methods-GeneDB1.R to call function GeneDB1
##A faire dans DESCRIPTION !!
##source("rRice/R/methods-GeneDB1.R")
##source("rRice/R/methods-GeneDB3.R")
##source("rRice/R/functions-basics.R")
##source("rRice/R/AllClasses.R")

command ="python3"


#' callDB1
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB1. All these locus will be stocked in listGenes
#' 
#' @param locusList list of locus for which we want the genes
#' @export
#' @import jsonlite
#' @importFrom jsonlite fromJSON
#' @rdname callDB1-function
callDB1 <- function (locusList) {
    
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    path2Script = paste(c(debut,path), collapse = '')
    
    listGenes <- data.frame()
    
    for (i in 1 : nrow(locusList)) {
        ch = as.character(locusList[i,1])
        start = as.character(locusList[i,2])
        end = as.character(locusList[i,3])
        
        ##appel du script python run.py avec les attributs (chx, start, end, DB) 
        ##-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "1")
        allArgs = c(path2Script, args)
        rOutput = system2(command, args=allArgs, stdout=TRUE)
        ##print(rOutput)
        
        if (rOutput != "empty") {
            jsonOutput <- fromJSON(rOutput)
            
            ##for (j in 1:length(jsonOutput)){
            ##    print(paste("numero",j," -> ",jsonOutput[j][1]))
            ##}
            
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

            if (!existsGene(listGenes,as.character(idRec))) {
                newGene <- GeneDB1(as.character(idRec),
                                   locusList[i,],
                                   as.character(rapName),
                                   as.character(rapSymbol),
                                   as.character(cgsnlName),
                                   as.character(cgsnlGene),
                                   as.character(oryGeneName),
                                   as.character(oryGeneSymbol),
                                   positionData,
                                   as.character(description))
                ##print(id_rec)
                listGenes <- append(listGenes,newGene)
            }
        }
    }
    return (listGenes)
}

#' callDB3
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB3. All these locus will be stocked in listGenes
#' 
#' @param locusList list
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname callDB3-function
callDB3 <- function (locusList) {
    
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    path2Script = paste(c(debut,path), collapse = '')
    
    listGenes <- data.frame()
    
    for (i in 1 : nrow(locusList)) {
        ch = as.character(locusList[i,1])
        start = as.character(locusList[i,2])
        end = as.character(locusList[i,3])
        
        #appel du script python run.py avec les attributs (chx, start, end, DB) 
        #-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "3")
        allArgs = c(path2Script, args)
        rOutput = system2(command, args=allArgs, stdout=TRUE)
        print(rOutput)
        
        if (rOutput != "not found") {
            jsonOutput <- fromJSON(rOutput)
            
            for (j in 1:length(jsonOutput)){
                print(paste("numero",j," -> ",jsonOutput[j][1]))
            }
            
            
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
                #print(id_rec)
                listGenes <- append(listGenes,newGene)
            }
        }
        else {
            print("not found")
        }
    }
    return (listGenes)
}

#phase de jsonOutput
#data <- data.frame(ch = c("1","1","1","1","1"),
#                   st = c("148907","5671734","9344261","10225320","148907"),
#                   end = c("248907","6337629","11332201","10325320","248907"))

#data <- data.frame(ch = c("1","1","1"),
#                   st = c("148907","5671734","5671734"),
#                   end = c("248907","6337629","6337629"))

#data <- data.frame(ch = c("1"),
#                   st = c("148907"),
#                   end = c("248907"))

#print(data)

#callDB1(data)
