#' Get the properties you want from the genes you want
#'
#' Get the properties you want from the genes you want
#'
#' @param geneList number
#' @param attributesVector number
#' @return it will return a dataframe with all the informations you want
#' @export
#' @rdname getProperties-function 
getProperties <- function(geneList, attributesVector){
    if(length(geneList) == 0){
        stop("your gene list is empty")
    }
    if(!inherits(geneList[[1]],c("Gene"))){
        stop("you are not giving a list of genes")
    }
    properties <- data.frame()
    props <- purrr::map(attributesVector,function(x){
        obj <- strsplit(x,"/")
        if(length(obj[[1]]) == 2){
            properties <- data.frame()
            className <- obj[[1]][[1]]
            attributeName <- obj[[1]][[2]]
            resList <- c()
            resList <- purrr::map(geneList,
                                  function(x, class, attribute, resList){
                if(class(x) == class){
                    properties <- attributes(x)
                    resList <- append(resList, properties[attribute])
                    print(resList)
                    print(properties)
                }else{
                    resList <- append
                }
            }, className, attributeName, resList)
            properties <- append(properties, resList)
        }else{
            stop("attribute not in the good format")
        }

    })
    properties <- purrr::map(props,function(x, properties){
        properties <<- append(properties, x)
    }, properties)
    names(properties) 
    return(properties)
}
