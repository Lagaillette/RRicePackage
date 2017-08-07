# getProperties <- function(geneVector, attributesVector){
#     if(length(geneVector) == 0){
#         stop("your gene list is empty")
#     }
#     if(!inherits(geneVector[[1]],c("Gene"))){
#         stop("you are not giving a list of genes")
#     }
#     properties <- data.frame()
#     props <- purrr::map(attributesVector,function(x){
#         obj <- strsplit(x,"/")
#         if(length(obj[[1]]) == 2){
#             properties <- data.frame()
#             className <- obj[[1]][[1]]
#             attributeName <- obj[[1]][[2]]
#             resList <- c()
#             resList <- purrr::map(geneVector,
#                                   function(x, class, attribute, resList){
#                 if(class(x) == class){
#                     properties <- attributes(x)
#                     resList <- append(resList, properties[attribute])
#                     print(resList)
#                     print(properties)
#                 }else{
#                     resList <- append
#                 }
#             }, className, attributeName, resList)
#             properties <- append(properties, resList)
#         }else{
#             stop("attribute not in the good format")
#         }
# 
#     })
#     properties <- purrr::map(props,function(x, properties){
#         properties <<- append(properties, x)
#     }, properties)
#     names(properties)
#     return(properties)
# }

#' Get the properties you want from the genes you want
#'
#' Get the properties you want from the genes you want
#'
#' @param geneList list
#' @param attributesVector vector
#' @return it will return a dataframe with all the informations you want
#' @import purrr
#' @export
#' @rdname getProperties-function
getProperties <- function(geneList, attributesVector){
    if(length(geneList) == 0){
        stop("your gene list is empty")
    }
    if(!inherits(geneList[[1]][[1]],c("Gene"))){
        stop("you are not giving a list of genes")
    }
    result <- purrr::map(geneList[[1]], function(x){
        new("Gene", id=x@id, genesIDs=x@genesIDs, locus=x@locus)
    })
    i <- 1
    j <- 1
    while(j <= length(geneList[[1]])){
        while (i <= length(geneList)){
            for(k in 1:length(attributesVector)){
                obj <- strsplit(attributesVector[[k]],"/")
                className <- obj[[1]][[1]]
                attributeName <- obj[[1]][[2]]
                if(class(geneList[[i]][[j]]) == className){
                    if(k == 2){
                        print("wtf")
                    }
                    properties <- attributes(geneList[[i]][[j]])
                    result[[j]]@others <- append(result[[j]]@others,
                                                 properties[attributeName])
                }
            }
            i <- i+1
        }
        j <- j+1
        i <- 1
    }
    return(result)
}

