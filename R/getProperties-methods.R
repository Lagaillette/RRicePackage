#' @rdname getProperties-methods
#' @aliases getProperties,Experiment-method
setMethod(
    "getProperties",
    signature = "Experiment",
    definition = function(object, idWanted){
        geneList <- object@genes
        attributesVector <- object@properties
        if(length(geneList) == 0){
            stop("your gene list is empty")
        }
        if(!inherits(geneList[[1]][[1]],c("Gene"))){
            stop("you are not giving a list of genes")
        }
        if(idWanted != "RAPDB" && idWanted != "MSU7"){
            stop("you have to give RAPDB or MSU7 as idWanted parameter")
        }
        if(length(attributesVector) == 0){
            return(object@genes)
        }else{
            result <- matrix(nrow=length(geneList[[1]]),
                             ncol=length(attributesVector))
            result <- as.data.frame(result)
            i <- 1
            j <- 1
            while(j <= length(geneList[[1]])){
                while (i <= length(geneList)){
                    for(k in 1:length(attributesVector)){
                        obj <- strsplit(attributesVector[[k]],"[.]")
                        className <- obj[[1]][[1]]
                        attributeName <- obj[[1]][[2]]
                        if(class(geneList[[i]][[j]]) == className){
                            properties <- attributes(geneList[[i]][[j]])
                            properties <- properties[attributeName]
                            properties <- properties[[1]]
                            result[[k]][[j]] <- properties[[1]]
                        }
                    }
                    i <- i+1
                }
                j <- j+1
                i <- 1
            }
            names(result) <- tolower(attributesVector)
            return(result)
        }
        
         
    }
)