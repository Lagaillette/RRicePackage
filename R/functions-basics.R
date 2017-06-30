existsGene <- function(genes, id){
    found <- FALSE
    i <- 1
    while(!found && i<length(genes)){
        if (genes[i]@id == id){
            found <- TRUE
        }
        i <- i+1
    }
    return(found)
}