#' creationGeneDB1
#'
#' This function is called only by callDB1 and will create the gene DB1
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB1
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB1-function
creationGeneDB1 <- function (x, y, IdsList, locusList) {
    ##PATH for package when it will be installed -> when it will be released
    path <- system.file("python/rricebeta",
                        "run.py",
                        package = "rRice")
    
    ##manage the spaces -> for example "Program Files" under windows will 
    ##generate an error because with system2 we generate a command line
    ##with multiple arguments in one string. 
    if (Sys.info()["sysname"] == "Windows"){
        path <- shortPathName(path)
    }
    
    id <- IdsList[[x]][[y]]
    id <- as.character(id)
    
    ch = as.character(locusList[x,1])
    start = as.character(locusList[x,2])
    end = as.character(locusList[x,3])
    
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "1", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "1", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))
            
            rOutput[sapply(rOutput, is.null)] <- NULL
            
            ##print(rOutput)
            
            ##if rOutput is not an empty list then we don't create a new GeneDB1
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
                
                if (position != "") {
                    position <- as.character(position)
                    pos1 <- strsplit(position, ":")
                    pos2 <- pos1[[1]][[2]]
                    pos3 <- strsplit(pos2,"[..]")
                    
                    positionData <- data.frame(ch=c(pos1[[1]][[1]]),
                                               st=c(pos3[[1]][[1]]),
                                               end=c(pos3[[1]][[3]]))
                }
                else {
                    positionData <- data.frame()
                }
                
                
                ##dataLocus <- data.frame(ch = ch, st = start, end = end)
                dataLocus <- data.frame(ch = as.character(locusList[x,1]),
                                        st = as.character(locusList[x,2]),
                                        end = as.character(locusList[x,3]))
                
                newGene <- new("GeneDB1",
                               id = as.character(idRec),
                               locus = dataLocus,
                               others = list(),
                               rapDBGeneNameSynonym = as.character(rapName),
                               rapDBGeneSymbolSynonym = as.character(rapSymbol),
                               cgsnlGeneName = as.character(cgsnlName),
                               cgsnlGeneSymbol = as.character(cgsnlGene),
                               oryzabaseGeneNameSynonym = 
                                   as.character(oryGeneName),
                               oryzabaseGeneSymbolSynonym = 
                                   as.character(oryGeneSymbol),
                               position = positionData,
                               description = as.character(description))
                
                return(newGene)
            }
        }
    }
    
}

#' callCreationGeneDB1
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB1
#' @rdname callCreationGeneDB1-function
callCreationGeneDB1 <- function (x, IdsList, locusList) {
    listGenes1 <- data.frame()
    
    listGenes1 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB1(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes1)
}

#' callDB1
#'
#' This function is the new function which will replace callDB1 function
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB1
#' @export
#' @rdname callDB1-function
#' @examples 
#' locusList <- data.frame(ch = c("1"),
#'                         st = c("148907"),
#'                         end = c("248907"))
#'                         
#' ids <- list(list("Os01g0102700"))
#'                 
#' callDB1(ids, locusList)
callDB1 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB1(x, 
                                                                  IdsList, 
                                                                  locusList))
        
        
        ##Remove all the NULL object from the list
        listGenes[sapply(listGenes, is.null)] <- NULL
        
        ##To delete all the geneDB1 which exists in double
        listGenes <- unique(listGenes)
        
        ##liste is a list with only the genes. 
        liste <- list()
        lapply(1 : length(listGenes),
               FUN = function(x){liste <<- append(liste,listGenes[[x]])})
        
        return (liste)
    }
    else {
        stop("IdsList has to be a list")
    }
}

######################

#' creationGeneDB2
#'
#' This function is called only by callDB2 and will create the gene DB2
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB2
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB2-function
creationGeneDB2 <- function (x, y, IdsList, locusList) {
    ##PATH for package when it will be installed -> when it will be released
    path <- system.file("python/rricebeta",
                        "run.py",
                        package = "rRice")
    
    ##manage the spaces -> for example "Program Files" under windows will 
    ##generate an error because with system2 we generate a command line
    ##with multiple arguments in one string. 
    if (Sys.info()["sysname"] == "Windows"){
        path <- shortPathName(path)
    }
    
    ch = as.character(locusList[x,1])
    start = as.character(locusList[x,2])
    end = as.character(locusList[x,3])
    
    
    
    id <- IdsList[[x]][[y]]
    id <- as.character(id)
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "2", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "2", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))
            
            rOutput[sapply(rOutput, is.null)] <- NULL
            
            ##print(rOutput)
            
            if (length(rOutput) > 0) {
                jsonOutput <- fromJSON(rOutput[[1]])
                
                ##Few informations BUT NOT ALL!! See with Julien and Hai what
                ##to take from all the informations
                idRec = jsonOutput["_id"]
                description = jsonOutput["description"]
                biotype = jsonOutput["biotype"]
                taxon_id = jsonOutput["taxon_id"]
                system_name = jsonOutput["system_name"]
                db_type = jsonOutput["db_type"]
                gene_idx = jsonOutput["gene_idx"]
                location_region = jsonOutput["location"]$location$region
                location_start = jsonOutput["location"]$location$start
                location_end = jsonOutput["location"]$location$end
                # = jsonOutput[""]
                
                # print(paste0(idRec," ",
                #       description," ",
                #       biotype," ",
                #       taxon_id," ",
                #       system_name," ",
                #       db_type," ",
                #       gene_idx," "
                #       ))
                # print(paste0(location_region," ",
                #              location_start," ",
                #              location_end))
            }
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
    
    
}

#' callCreationGeneDB2
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB2
#' @rdname callCreationGeneDB2-function
callCreationGeneDB2 <- function (x, IdsList, locusList) {
    listGenes2 <- data.frame()
    
    listGenes2 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB2(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes2)
}

#' callDB2
#'
#' This function will call the Gramene database
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB2
#' @export
#' @rdname callDB2-function
#' @examples 
#' locusList <- data.frame(ch = c("1"),
#'                         st = c("148907"),
#'                         end = c("248907"))
#'                         
#' ids <- list(list("Os01g0102700"))
#'                 
#' callDB2(ids, locusList)
callDB2 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB2(x, 
                                                                  IdsList, 
                                                                  locusList))
        
        
        ##Remove all the NULL object from the list
        listGenes[sapply(listGenes, is.null)] <- NULL
        
        ##To delete all the geneDB1 which exists in double
        listGenes <- unique(listGenes)
        
        ##liste is a list with only the genes. 
        liste <- list()
        lapply(1 : length(listGenes),
               FUN = function(x){liste <<- append(liste,listGenes[[x]])})
        
        return (liste)
    }
    else {
        stop("IdsList has to be a list")
    }
}



#############################################################################

#' creationGeneDB3
#'
#' This function is called only by callDB3 and will create the gene DB3
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB3
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB3-function
creationGeneDB3 <- function (x, y, IdsList, locusList) {
    ##PATH for package when it will be installed -> when it will be released
    path <- system.file("python/rricebeta",
                        "run.py",
                        package = "rRice")
    
    ##manage the spaces -> for example "Program Files" under windows will 
    ##generate an error because with system2 we generate a command line
    ##with multiple arguments in one string. 
    if (Sys.info()["sysname"] == "Windows"){
        path <- shortPathName(path)
    }
    
    ch = as.character(locusList[x,1])
    start = as.character(locusList[x,2])
    end = as.character(locusList[x,3])
    
    
    
    id <- IdsList[[x]][[y]]
    id <- as.character(id)
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "3", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "3", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))
            
            rOutput[sapply(rOutput, is.null)] <- NULL
            
            ##print(rOutput)
            
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
                
                newGene <- new("GeneDB3",
                               id = "",
                               locus = locusList[x,],
                               others = list(),
                               traitGeneId = as.character(traitGeneId),
                               cgsnlGeneSymbol = as.character(cgsnlGeneSymbol),
                               GeneSymbolSynonim = 
                                   as.character(geneSymbolSynonym),
                               cgsnlSymbolSynonim = as.character(cgsnlGeneName),
                               GeneNameSynonim = as.character(geneNameSynonym),
                               proteinName = as.character(proteinName),
                               allele = as.character(allele),
                               chromosomeNumber = as.numeric(chromosomeNo),
                               explanation = as.character(explanation),
                               traitClass = as.character(traitClass),
                               rapID = as.character(rapId),
                               grameneId = as.character(grameneId),
                               arm = as.character(arm),
                               locate = as.character(locateCm),
                               geneOntology = as.character(geneOntology),
                               traitOntology = as.character(traitOntology),
                               plantOntology = as.character(plantOntology))
                
                return(newGene)
            }
        }
        else {
            stop("One of your locus has a problem")
        }
    }
}

#' callCreationGeneDB3
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB3
#' @rdname callCreationGeneDB3-function
callCreationGeneDB3 <- function (x, IdsList, locusList) {
    listGenes3 <- data.frame()
    
    listGenes3 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB3(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    #listGenes3[sapply(listGenes3, is.null)] <- NULL
    
    return(listGenes3)
}

#' callDB3
#'
#' This function will call the third database
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB3
#' @export
#' @rdname callDB3-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("idTest"))
#'                 
#' callDB3(ids, locusList)
callDB3 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        if (!is.na(IdsList[[1]][[1]])){
            if (IdsList[[1]][[1]] == "idTest"){
                return(list())
            }
            else {
                ##We call the function creationGeneDB1 to create our newGene
                listGenes <- lapply(1 : length(IdsList),
                                    FUN = function(x) 
                                        callCreationGeneDB3(x, 
                                                            IdsList, 
                                                            locusList))
                
                
                ##Remove all the NULL object from the list
                listGenes[sapply(listGenes, is.null)] <- NULL
                
                ##To delete all the geneDB1 which exists in double
                listGenes <- unique(listGenes)
                
                ##liste is a list with only the genes. 
                liste <- list()
                lapply(1 : length(listGenes),
                       FUN = function(x){
                           liste <<- append(liste,listGenes[[x]])})
                
                ##Remove all the NULL object from the list
                liste[sapply(liste, is.null)] <- NULL
                
                return (liste)
            }
        }
    }
    else {
        stop("IdsList has to be a list")
    }
        
}

#############################################################################

