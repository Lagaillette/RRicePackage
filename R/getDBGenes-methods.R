#' @rdname getDBGenes-methods
#' @aliases getDBGenes,Experiment-method
#' @examples 
#' gene1 <- new("RAPDB")
#' listGenes <- list(gene1)
#' exp <- new(Class="Experiment",
#' name="test",
#' date=Sys.Date(),
#' genes=listGenes,
#' databases=list(1),
#' others=list())
#' getDBGenes(exp,1)
#' 
setMethod(
    "getDBGenes",
     signature = "Experiment",
     definition = function(object, nbDB){
         find <- FALSE
         databases <- object@databases
         pos <- NULL
         it <- 1
         while(it <= length(databases) && !find){
             if(identical(databases[[it]],nbDB)){
                 pos <- it
                 find <- TRUE
             }
             it <- it+1
         }
         if(find){
             return(object@genes[[pos]])
         }
          else{
              stop("the database you want to access is not available")
          }
})