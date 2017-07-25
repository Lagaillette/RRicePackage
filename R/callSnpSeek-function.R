library(jsonlite)

#' id
#'
#' This function will only return the id from the rOutput 
#' 
#' @param rOutput character
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname id-function
id <- function (rOutput) {
    #output <- getOutPutJSON(rOutput)
    #print(rOutput)
    
    #t <- '{"msu7Name": "LOC_Os01g01970"}' # --> BON
    #t <- "{'msu7Name': 'LOC_Os01g01970'}" # --> PAS BON POUR JSON
    # rOutput <- {'contig': 'chr01',
    #             'fmin': 524578, 
    #             'rappredName': 'None', 
    #             'fmax': 528002, 
    #             'strand': -1, 
    #             'uniquename': 'LOC_Os01g01970', 
    #             'fgeneshName': 'chr01-gene_91', 
    #             'msu7Name': 'LOC_Os01g01970', 
    #             'description': 'expressed protein', 
    #             'raprepName': 'Os01g0109750', 
    #             'iricname': 'OsNippo01g015950'}
    
    ##A VOIR AVEC BAPTISTE POUR QUE JE RECOIVE UN BON JSON
    rOutput <- gsub('\'', '"', rOutput)
    rOutput <- gsub('None', '"None"', rOutput)
    
    #print(j['msu7Name'])
    jsonOutput <- fromJSON(rOutput)
    if (jsonOutput['rappredName'] == "None"){
        id <- jsonOutput['raprepName']
    }
    else if (jsonOutput['rapredName'] == "None") {
        id <- jsonOutput['rappredName']
    }
    else {
        id <- jsonOutput['raprepName']
    }
    
    if (id != "None") {
        return (id)
    }
    
}

#' getIds
#'
#' This function is called for each locus and has to return the list of ids 
#' present in the locus
#' 
#' @param i number
#' @param locusList list
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname getIds-function
getIds <- function (i, locusList) {
    ##PATH for package when it will be installed -> when it will be released
    path <- system.file("python/rricebeta",
                        "run.py",
                        package = "rRice")
    
    listIds <- data.frame()
    
    #path2Script = paste(c(path), collapse = '')
    
    ch = as.character(locusList[i,1])
    start = as.character(locusList[i,2])
    end = as.character(locusList[i,3])
    
    if (ch != "" && start != "" && end != "") {
        ##appel du script python run.py avec les attributs (chx, start, end, DB) 
        ##-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "call_snpSeek", "None")
        #allArgs = c(path2Script, args)
        
        #rOutput = system2(command, args=allArgs, stdout=TRUE)
        rOutput = system2(command=path, args=args, stdout=TRUE)
        
        rOutput <- lapply(1 : length(rOutput),
                          function(x) getOutPutJSON(rOutput[x]))
        
        rOutput[sapply(rOutput, is.null)] <- NULL
        
        ##print(rOutput)
        
        
        if (length(rOutput) > 0) {
            #print(rOutput)
            #jsonOutput <- fromJSON(rOutput[[1]])
            
            listIds <- lapply(1 : length(rOutput),
                              FUN = function(x) id(rOutput[[x]]))
        }
        
        ##Remove all the NULL object from the list
        listIds[sapply(listIds, is.null)] <- NULL
        
        ##print(listIds)
        return (listIds)
    }
    else {
        return (list())
    }
    
}

#' Function for recup a list of ids
#' 
#' 
#' @param locus the list of locus for which we want the ids
#' @return the list of id's of each genes we want
#' @export
#' @rdname callSnpSeek-function
callSnpSeek <- function(locus){

        listIds <- data.frame()
        
        if (length(locus) > 0) {
            ##We call the function creationGeneDB1 to create our newGene
            listIds <- lapply(1 : nrow(locus),
                              FUN = function(x) getIds(x, locus))
            
            
            ##Remove all the NULL object from the list
            listIds[sapply(listIds, is.null)] <- NULL
            
            ##To delete all the geneDB1 which exists in double
            listIds <- unique(listIds)
            
            ##print(listIds)
            return (listIds)
        }
        else {
            return (list())
        }
    
}


# data <- data.frame(ch = c("1"),
#                    st = c("527906"),
#                    end = c("842359"))
# 

#data <- data.frame(ch = c("1","1","1"),
#                   st = c("148907","9344261","527906"),
#                   end = c("248907","11332201","842359"))

# print(data)
# s <- callSnpSeek(data)
#print(s)