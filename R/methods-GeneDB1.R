###############################################################################
################################ Constructor ##################################


GeneDB1 <- function(uniquename, locus, msU7name, fgeneshName, rappredname,
                    fmin, fmax,contig,iricname, strand, description){
    
    result <- new("GeneDB1",
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
