################################  DB1 (RAPDB) - works #########################

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
    id <- as.character(id[[1]])
    
    ##print(id)
    
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
                
                newGene <- new("RAPDB",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
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

################################  DB2 (GRAMENE) - #######################

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
    id <- as.character(id[[1]])
    
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
                taxonId = jsonOutput["taxon_id"]
                systemName = jsonOutput["system_name"]
                dbType = jsonOutput["db_type"]
                geneIdx = jsonOutput["gene_idx"]
                loc_region = jsonOutput["location"]$location$region
                loc_start = jsonOutput["location"]$location$start
                loc_end = jsonOutput["location"]$location$end
                loc_strand = jsonOutput["location"]$location$strand
                loc_map = jsonOutput["location"]$location$map
                # = jsonOutput[""]
                
                location <- data.frame(region = c(as.character(loc_region)),
                                       start = c(as.character(loc_start)),
                                       end = c(as.character(loc_end)),
                                       strand = c(as.character(loc_strand)),
                                       map = c(as.character(loc_map))
                )

                newGene <- new("Gramene",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               description = as.character(description),
                               biotype = as.character(biotype),
                               taxonId = as.character(taxonId),
                               systemName = as.character(systemName),
                               dbType = as.character(dbType),
                               geneIdx = as.character(geneIdx),
                               location = location
                )

                return (newGene)
                
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
                

                
                # data2 <- data.frame(ch = c("1","1"),
                #                     st = c("148907","527906"),
                #                     end = c("248907","842359"))
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



##################################  DB3 (ORYZABASE) - #########################

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
    id <- as.character(id[[1]])
    
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
            
            print(rOutput)
            
            if (length(rOutput) > 0) {
                rOutput <- gsub('\'', '"', rOutput)
                jsonOutput <- fromJSON(rOutput)
                
                traitGeneId = jsonOutput["Trait Gene Id"]
                cgsnlGeneSymbol =jsonOutput["CGSNL Gene Symbol"]
                geneSymbolSynonym =jsonOutput["Gene symbol synonym(s)"]
                cgsnlGeneName = jsonOutput["CGSNL Gene Name"]
                geneNameSynonym =jsonOutput["Gene name synonym(s)"]
                proteinName =jsonOutput["Protein Name"]
                allele = jsonOutput["Allele"]
                chromosomeNo = jsonOutput["Chr. No."]
                explanation =jsonOutput["Explanation"]
                traitClass =jsonOutput["Trait Class"]
                rapId =jsonOutput["RAP ID"]
                grameneId =jsonOutput["Gramene ID"]
                arm =jsonOutput["Arm"]
                locateCm = jsonOutput["Locate(cM)"]
                geneOntology = jsonOutput["Gene Ontology"]
                traitOntology = jsonOutput["Trait Ontology"]
                plantOntology = jsonOutput["Plant Ontology"]
                
                newGene <- new("Oryzabase",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
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
        if (length(IdsList) != 0){
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

################################  DB4 (IC4R) - works but very long data #######

#' makeList
#'
#' This function is called in order to return elemnt which will form a list 
#'
#' @param liste list
#' @param number number
#' @return it will return ... 
#' @importFrom methods new
#' @rdname makeList-function
makeList <- function (liste, number){
    #print(liste[[1]])
    if (number == 1)
        return(liste[[1]][[1]])
    else if (number == 2)
        return(liste[[1]][[2]])
    else if (number == 3)
        return(liste[[1]][[3]])
    else if (number == 4)
        return(liste[[1]][[4]])
    else if (number == 5)
        return(liste[[1]][[5]])
    else if (number == 6)
        return(liste[[1]][[6]])
    else if (number == 7)
        return(liste[[1]][[7]])
    else
        stop("problem with the number which has to be between 1 and 7")
    
}

#' analyseDataDB7
#'
#' This function is called 
#'
#' @param rOutput character
#' @return it will return ... 
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' @rdname analyseDataDB7-function
analyseDataDB4 <- function (rOutput) {
    if (length(rOutput) > 0 && rOutput != "None") {
        #Format JSON not good !!
        rOutput <- gsub('\'', '"', rOutput)
        
        ##Transform in order to use data
        jsonOutput <- fromJSON(rOutput[[1]])
        #print(rOutput)
        # = jsonOutput[""]
        #print(jsonOutput)
        
        experiment_name = jsonOutput["Experiment name"]
        dev_stage = jsonOutput["Development stage"]
        experiment_id = jsonOutput["Experiment ID"]
        variety = jsonOutput["Variety"]
        project_id = jsonOutput["Project ID"]
        tissue = jsonOutput["Tissue"]
        expr_value = jsonOutput["Expression value"]
        
        # print(paste0(experiment_name," ",
        #              dev_stage," ",
        #              experiment_id," ",
        #              variety," ",
        #              project_id," ",
        #              tissue," ",
        #              expr_value))
        
        #print(experiment_id)
        #print(dev_stage)
        #print(experiment_name)
        #print(variety)
        #print(project_id)
        #print(tissue)
        #print(expr_value)
        
        return(list(experiment_name,
                    dev_stage,
                    experiment_id,
                    variety,
                    project_id,
                    tissue,
                    expr_value))
        
        
    }
}

#' creationGeneDB4
#'
#' This function is called only by callDB4 and will create the gene DB4
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB4
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB4-function
creationGeneDB4 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[1]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "4", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "4", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            # rOutput <- lapply(1 : length(rOutput),
            #                   function(x) getOutPutJSON(rOutput[x]))
            # 
            # rOutput[sapply(rOutput, is.null)] <- NULL

            
            
            # r <- rOutput[3]
            # r <- substring(r,2)
            # #r <- substring(r, 1, (length(r)-1))
            # r <- substr(r, 1, nchar(r)-1)
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))

            rOutput[sapply(rOutput, is.null)] <- NULL
            
            # print(rOutput[1])
            # print(rOutput[2])
            # print(rOutput[3])
            # print(rOutput[4])
            # print(rOutput[5])
            
            if (length(rOutput) > 0) {
                rOutput <- lapply(1 : length(rOutput),
                                  function(x) analyseDataDB4(rOutput[x]))
                ##print(length(rOutput))
                      
                experimentName <- lapply(1 : length(rOutput),
                                         function(x) makeList(rOutput[x],1))
                devStage <- lapply(1 : length(rOutput),
                                   function(x) makeList(rOutput[x],2))
                experimentId <- lapply(1 : length(rOutput),
                                       function(x) makeList(rOutput[x],3))
                variety <- lapply(1 : length(rOutput),
                                  function(x) makeList(rOutput[x],4))
                projectId <- lapply(1 : length(rOutput),
                                    function(x) makeList(rOutput[x],5))
                tissue <- lapply(1 : length(rOutput),
                                 function(x) makeList(rOutput[x],6))
                exprValue <- lapply(1 : length(rOutput),
                                    function(x) makeList(rOutput[x],7))

                ##print(exprValue)
                
                newGene <- new("IC4R",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               experimentName = experimentName,
                               devStage = devStage,
                               experimentId = experimentId,
                               variety = variety,
                               projectId = projectId,
                               tissue = tissue,
                               exprValue = exprValue)
                
                return (newGene)
            }
   
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
    
    
}

#' callCreationGeneDB4
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB4
#' @rdname callCreationGeneDB4-function
callCreationGeneDB4 <- function (x, IdsList, locusList) {
    listGenes4 <- data.frame()
    
    listGenes4 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB4(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes4)
}

#' callDB4
#'
#' This function will call the Gramene database
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB4
#' @export
#' @rdname callDB4-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB4(ids, locusList)
callDB4 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB4(x, 
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

#############################  DB5 (PLANTTFDB) - STAND BY  ####################
#' creationGeneDB5
#'
#' This function is called only by callDB8 and will create the gene DB5
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB5
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB5-function
creationGeneDB5 <- function (x, y, IdsList, locusList) {
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
                args = c(path, ch, start, end, "5", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "5", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            # rOutput <- lapply(1 : length(rOutput),
            #                   function(x) getOutPutJSON(rOutput[x]))
            # 
            # rOutput[sapply(rOutput, is.null)] <- NULL
            
            print(rOutput)
            
            # if (length(rOutput) > 0) {
            #     jsonOutput <- fromJSON(rOutput[[1]])
            #     
            #     # = jsonOutput[""]
            #     
            # }
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB5
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB5
#' @rdname callCreationGeneDB5-function
callCreationGeneDB5 <- function (x, IdsList, locusList) {
    listGenes5 <- data.frame()
    
    listGenes5 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB5(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes5)
}

#' callDB5
#'
#' This function will call the Gramene database
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB5
#' @export
#' @rdname callDB5-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB5(ids, locusList)
callDB5 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB5(x, 
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

###################################  DB6 (PLNTFDB) Works ######################
#' creationGeneDB6
#'
#' This function is called only by callDB6 and will create the gene DB6
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB6
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB6-function
creationGeneDB6 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[2]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "6", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "6", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            ##rOutput is an array so don't use getOutputJSON
            ##print(class(rOutput))
            ##print(rOutput[1])
            ##print(rOutput[2])
            #id <- rOutput[1]
            family <- rOutput[1]
            #print(paste(id,family))
            
            newGene <- new("PLNTFDB",
                           id = as.character(id),
                           genesIDs = as.list(IdsList[[x]][[y]]),
                           locus = locusList[x,],
                           others = list(),
                           family = as.character(family))
            
            return (newGene)
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB6
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB6
#' @rdname callCreationGeneDB6-function
callCreationGeneDB6 <- function (x, IdsList, locusList) {
    listGenes6 <- data.frame()
    
    listGenes6 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB6(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes6)
}

#' callDB6
#'
#' This function will call 
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB6
#' @export
#' @rdname callDB6-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB6(ids, locusList)
callDB6 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB6(x, 
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


###################################  DB7 (FUNRICEGENES)- works  ###############
#' creationGeneDB7
#'
#' This function is called only by callDB7 and will create the gene DB7
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB7
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB7-function
creationGeneDB7 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[1]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "7", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "7", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            ##rOutput is an array so don't use getOutputJSON
            ##print(class(rOutput))
            #print(rOutput)
            # print(rOutput[1])
            # print(rOutput[2])
            # print(rOutput[3])
            #print(rOutput)
            # symbol <- rOutput[1]
            # #print(symbol)
            # if (symbol == "Series([], Name: Symbol, dtype: object)" 
            #     || is.na(symbol)){
            #     symbol <- "None"
            # }
            # else {
            #     symbol <- strsplit(symbol," ")
            #     symbol <- symbol[[1]][[length(symbol[[1]])]]
            # }
            # #print(symbol)
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))
            
            rOutput[sapply(rOutput, is.null)] <- NULL
            
            ##print(rOutput)
            
            ##if rOutput is not an empty list then we don't create a new GeneDB1
            if (length(rOutput) > 0) {
                rOutput <- gsub('\'', '"', rOutput)
                
                jsonOutput <- fromJSON(rOutput)
                
                ##print(jsonOutput)
                symbol <- jsonOutput['Symbol']

                newGene <- new("Funricigenes",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               symbol = as.character(symbol))

                return (newGene)
            }
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB7
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB7
#' @rdname callCreationGeneDB7-function
callCreationGeneDB7 <- function (x, IdsList, locusList) {
    listGenes7 <- data.frame()
    
    listGenes7 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB7(x,
                                                            y,
                                                            IdsList,
                                                            locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes7)
}

#' callDB7
#'
#' This function will call 
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB7
#' @export
#' @rdname callDB7-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB7(ids, locusList)
callDB7 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB7(x, 
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


###################################  DB8 (FUNRICEGENES2) - works  #############
#' creationGeneDB8
#'
#' This function is called only by callDB8 and will create the gene DB8
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB8
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB8-function
creationGeneDB8 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[1]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "8", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "8", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            #print(rOutput)

            
            # str <- rOutput[1]
            # 
            # if (str == "Series([], dtype: object)" || is.na(str)){
            #     symbol <- "None"
            #     name <- "None"
            # }
            # else {
            #     str <- strsplit(str,"°")
            #     symbol <- str[[1]][[2]]
            #     name <- str[[1]][[3]]
            # }
            # print(paste(symbol, name))
            
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))

            rOutput[sapply(rOutput, is.null)] <- NULL

            if (length(rOutput) > 0) {
                rOutput <- gsub('\'', '"', rOutput)
                jsonOutput <- fromJSON(rOutput)

                symbol <- jsonOutput['Symbol']
                name <- jsonOutput['Name']

                newGene <- new("Funricigenes2",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               symbol = as.character(symbol),
                               name = as.character(name))

                return (newGene)
            }
            
            
            
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB8
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB8
#' @rdname callCreationGeneDB8-function
callCreationGeneDB8 <- function (x, IdsList, locusList) {
    listGenes8 <- data.frame()
    
    listGenes8 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB8(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes8)
}

#' callDB8
#'
#' This function will call 
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB8
#' @export
#' @rdname callDB8-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB8(ids, locusList)
callDB8 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB8(x, 
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

###################################  DB9  (FUNRICEGENES3) -   #################
#' creationGeneDB9
#'
#' This function is called only by callDB9 and will create the gene DB9
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB9
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB9-function
creationGeneDB9 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[1]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "9", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "9", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            ##print(rOutput)
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))

            rOutput[sapply(rOutput, is.null)] <- NULL

            if (length(rOutput) > 0) {
                rOutput <- gsub('\'', '"', rOutput)
                jsonOutput <- fromJSON(rOutput)

                symbol <- jsonOutput['Symbol']
                keyword <- jsonOutput['Keyword']
                title <- jsonOutput['Title']

                newGene <- new("Funricigenes3",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               symbol = as.character(symbol),
                               keyword = as.character(keyword),
                               title = as.character(title))

                return (newGene)
            }
            
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB9
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB9
#' @rdname callCreationGeneDB9-function
callCreationGeneDB9<- function (x, IdsList, locusList) {
    listGenes9 <- data.frame()
    
    listGenes9 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB9(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes9)
}

#' callDB9
#'
#' This function will call 
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB9
#' @export
#' @rdname callDB9-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB9(ids, locusList)
callDB9 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB9(x, 
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

###################################  DB10  (MSU) -   #################
#' creationGeneDB10
#'
#' This function is called only by callDB10 and will create the gene DB10
#' It will call run.py script which will return the list of the genes which
#' are present in the locus
#'
#' @param x number
#' @param y number
#' @param IdsList list
#' @param locusList list
#' @return it will return a GeneDB10
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @importFrom methods new
#' @rdname creationGeneDB10-function
creationGeneDB10 <- function (x, y, IdsList, locusList) {
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
    id <- as.character(id[[1]])
    
    if (ch != "" && start != "" && end != "") {
        if (id != "None") {
            ##Call run.py from python 
            if (Sys.info()["sysname"] == "Windows"){
                args = c(path, ch, start, end, "10", id)
                cmd <- findpython::find_python_cmd()
                rOutput = system2(command = cmd, args=args, stdout = TRUE)
            }
            else {
                args = c(ch, start, end, "10", id)
                rOutput = system2(command = path, args=args, stdout = TRUE)
            }
            
            ##print(rOutput)
            
            rOutput <- lapply(1 : length(rOutput),
                              function(x) getOutPutJSON(rOutput[x]))
            
            rOutput[sapply(rOutput, is.null)] <- NULL
            
            if (length(rOutput) > 0) {
                rOutput <- gsub('\'', '"', rOutput)
                jsonOutput <- fromJSON(rOutput)
                
                protein <- jsonOutput['Protein']
                genomicSequence <- jsonOutput['Genomic Sequence']
                cds <- jsonOutput['CDS']
              
                newGene <- new("MSU",
                               id = as.character(id),
                               genesIDs = as.list(IdsList[[x]][[y]]),
                               locus = locusList[x,],
                               others = list(),
                               protein = as.character(protein),
                               genomicSequence = as.character(genomicSequence),
                               cds = as.character(cds))

                return (newGene)
            }
            
        }
    }
    else {
        stop("One of the element of the locus is empty")
    }
}

#' callCreationGeneDB10
#'
#' This function ...
#'
#' @param x number
#' @param IdsList list
#' @param locusList list
#' @return It will return a list of genesDB10
#' @rdname callCreationGeneDB10-function
callCreationGeneDB10<- function (x, IdsList, locusList) {
    listGenes10 <- data.frame()
    
    listGenes10 <- lapply(1 : length(IdsList[[x]]),
                         FUN = function(y) creationGeneDB10(x,
                                                           y,
                                                           IdsList,
                                                           locusList))
    
    ##Remove all the NULL object from the list
    ##listGenes[sapply(listGenes, is.null)] <- NULL
    
    return(listGenes10)
}

#' callDB10
#'
#' This function will call 
#'
#' @param IdsList list of locus for which we want the genes
#' @param locusList list
#' @return It will return only a list with all the genesDB10
#' @export
#' @rdname callDB10-function
#' @examples 
#' locusList <- data.frame(ch = c("1","1"),
#'                         st = c("148907","527906"),
#'                         end = c("248907","842359"))
#'                         
#' ids <- list(list("Os01g0102700","Os01g0102800"),
#'             list("Os01g0109750","Os01g0110100"))
#'                 
#' callDB10(ids, locusList)
callDB10 <- function (IdsList, locusList) {
    
    listGenes <- data.frame()
    
    if (class(IdsList) == "list") {
        ##We call the function creationGeneDB1 to create our newGene
        listGenes <- lapply(1 : length(IdsList),
                            FUN = function(x) callCreationGeneDB10(x, 
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