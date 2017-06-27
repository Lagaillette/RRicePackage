###############################################################################
################################ Constructor ##################################


GeneDB1 <- function(id, uniquename, locus, msU7name, fgeneshName, rappredname,
                    fmin, fmax,contig,iricname, strand, description){
    
    result <- new("GeneDB1",
              id = id,
              uniquename = uniquename,
              locus = locus,
              others = list(),
              msU7name = msU7name,
              fgeneshName = fgeneshName,
              rappredname = rappredname,
              fmin = fmin,
              fmax = fmax,
              contig = contig,
              iricname = iricname,
              strand = strand,
              description = description)
    
    return(result)
          
}


###############################################################################
################################# Methods #####################################
