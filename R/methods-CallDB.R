library(jsonlite)
library(findpython)

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
#' @importFrom jsonlite fromJSON
#' @importFrom findpython find_python_cmd
#' @export
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
    print(id)
    
    ##for the ids like "Os01g0115500,Os01g0115566" (the double ids)
    ##we only test the first db
    if(grepl(',', id)) 
    {
        ids <- strsplit(id, ",")
        id <- ids[[1]][[1]]
        print(id)
        print(ids[[1]][[2]])
    }
    
    if (id != "None") {
        ##appel du script python run.py avec les attributs (chx, start, end, DB)
        ##-> tous les attributs doivent etre en chaine de carac
        if (Sys.info()["sysname"] == "Windows"){
            args = c(path, "None", "None", "None", "script7", id)
            cmd <- findpython::find_python_cmd()
            rOutput = system2(command = cmd, args=args, stdout = TRUE)
        }
        else {
            args = c("None", "None", "None", "script7", id)
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
                           oryzabaseGeneNameSynonym = as.character(oryGeneName),
                           oryzabaseGeneSymbolSynonym = as.character(oryGeneSymbol),
                           position = positionData,
                           description = as.character(description))
            
            return(newGene)
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
#' @export
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
#' @export
#' @rdname callDB1-function
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

# data <- data.frame(ch = c("1"),
#                    st = c("148907"),
#                    end = c("248907"))
# 
# print(data)
# s <- callSnpSeek(data)
# print(s)
# 
# d <- callDB1Bis(s)
# print(d)

######################

#' creationGeneDB3
#'
#' This function is called only by callDB3 and will create the gene DB3
#' It will call run.py script which will return the list of the genes which 
#' are present in the locus
#' 
#' @param i number
#' @param locusList list
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname creationGeneDB3-function
creationGeneDB3 <- function (i, locusList) {
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
    
    ch = as.character(locusList[i,1])
    start = as.character(locusList[i,2])
    end = as.character(locusList[i,3])
    
    if (ch != "" && start != "" && end != "") {
        ##appel du script python run.py avec les attributs (chx, start, end, DB) 
        ##-> tous les attributs doivent etre en chaine de carac
        if (Sys.info()["sysname"] == "Windows"){
            args = c(path, ch, start, end, "3", "None")
            cmd <- findpython::find_python_cmd()
            rOutput = system2(command = cmd, args=args, stdout = TRUE)
        }
        else {
            args = c(ch, start, end, "3", "None")
            rOutput = system2(command = path, args=args, stdout = TRUE)
        }
        
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
            
            newGene <- new("GeneDB3",
                           id = "",
                           locus = locusList[i,],
                           others = list(),
                           traitGeneId = as.character(traitGeneId),
                           cgsnlGeneSymbol = as.character(cgsnlGeneSymbol),
                           GeneSymbolSynonim = as.character(geneSymbolSynonym),
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
        return("One of your locus has a problem")
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
#                    st = c("9344261"),
#                    end = c("11332201"))

# data <- data.frame(ch = c("1"),
#                    st = c("148907"),
#                    end = c("248907"))

# data <- data.frame(ch = c("1","1"),
#                    st = c("148907","527906"),
#                    end = c("248907","842359"))
# 
# print(data)
# s <- callDB3(data)
# print(s)

# data <- data.frame(ch = c("1","1","1","1","1","1","1","1","1","1","1","1","1","1"),
#                   st = c("148907","5671734","9344261","9344261","10225320","10225320","15367095","21149478","21390962","22689596","34657419","34796909","34796909","39864172"),
#                   end = c("248907","6337629","11332201","11332201","10325320","10325320","17233103","22250712","21490962","22789596","35396321","34896909","34896909","41317992"))
# 
# data <- data.frame(ch = c("","","","","","","","","","","","","",""),
#                    st = c("","","","","","","","","","","","","",""),
#                    end = c("","","","","","","","","","","","","",""))

# data23 <- data.frame(ch = c("2","2","2","2","3","3","3"),
#                    st = c("6142704","25638752","26084899","30847375","31694633","35301755","35301755"),
#                    end = c("6965539","25738752","26883277","33746199","32833262","35401755","35401755"))

############################################

# a <- function () {
#     l <- list(list(1,2,3),list(4,5,6))
#     l1 <- list()
#     lapply(1 : length(l),
#                     function(x){
#                         l1 <<- append(l1,l[[x]])
#                     })
#     #b <- append(l[[1]],l[[2]])
#     print(l1)
# }

