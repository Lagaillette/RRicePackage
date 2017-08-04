getProperties <- function(geneList, AttributesVector){
    if(length(geneList) == 0){
        stop("your gene list is empty")
    }
    if(!inherits(geneList[[1]],c("Gene"))){
        stop("you are not giving a list of genes")
    }
    
}
