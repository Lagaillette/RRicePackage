#This function is used in methods-calldb to know if a gene belongs to the liste (genes)
existsGene <- function(genes, id){
    found <- FALSE
    i <- 1
    while(!found && i<=length(genes)){
        if (as.character(genes[[i]]@id) == as.character(id)){
            found <- TRUE
        }
        i <- i+1
    }
    return(found)
}


#this function allow us to know if the user is using Linux or Windows
whichone <- function(){
  found <- 0
  
  debut = getwd()
  if (substr(debut, 0,1) == '/') #ubuntu
    found <- 1
  else #windows
    found <- 0
  
  print(found)
  return(found)
}