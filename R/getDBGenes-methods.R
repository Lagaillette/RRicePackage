#' @rdname getDBGenes-methods
#' @aliases getDBGenes,Experiment-method
setMethod(
    "getDBGenes",
     signature = "Experiment",
     definition = function(object, nbDB){
         find <- FALSE
         databases <- object@databases
         pos <- NULL
         it <- 1
         while(it <= length(databases) && !find){
             if(identical(databases[[it]],nbDB){
                 pos <- it
                 find <- TRUE
             }
         }
         if(find){
             return(object@genes[[it]])
         }
          else{
              stop("the database you want to access is not available")
          }
})