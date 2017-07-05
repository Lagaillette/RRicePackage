library(jsonlite)

#on relie au fichier methods-GeneDB1.R to call function GeneDB1
#A faire dans DESCRIPTION !!
source("rRice/R/methods-GeneDB1.R")
source("rRice/R/methods-GeneDB3.R")
source("rRice/R/functions-basics.R")
source("rRice/R/AllClasses.R")

command ="python3"

#' This function calls Scriptv7 from python
#'
#' Cette fonction permet à l'utilisateur de chercher si le RAPID existe dans 
#' Oryzabase.txt
#' @param RAPID
#' @keywords 
#' @export
#' @examples
#' Appel_Scriptv7("Os06g0654600")
appel_Scriptv7 <- function (RAPID) {
  
    #chemin pour n'importe quel utilisateur
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"
    path2script = paste(c(debut,path), collapse = '')

    # Build up args in a vector
    # RAPID_valide = "Os06g0654600" -> exemple valide
    args = c(RAPID)

    # Add path to script as first arg
    allArgs = c(path2script, args)

    Routput = system2(command, args=allArgs, stdout=TRUE)

    #Appelé à chaque fois qu'elle rencontrera un print
    print(Routput)
}


#' callDB1
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB1. All these locus will be stocked in liste_genes
#' @param Locus list
#' @keywords 
#' @export
#' @examples
#' callDB1(Liste de locus)
callDB1 <- function (locusListe) {
  
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    path2script = paste(c(debut,path), collapse = '')
    
    liste_genes <- data.frame()
  
    for (i in 1 : nrow(locusListe)) {
        ch = as.character(locusListe[i,1])
        start = as.character(locusListe[i,2])
        end = as.character(locusListe[i,3])
    
        #appel du script python run.py avec les attributs (chx, start, end, DB) 
        #-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "1")
        allArgs = c(path2script, args)
        Routput = system2(command, args=allArgs, stdout=TRUE)
        #print(Routput)
        
        if (Routput != "empty") {
            json_output <- fromJSON(Routput)
            
            #for (j in 1:length(json_output)){
            #    print(paste("numero",j," -> ",json_output[j][1]))
            #}
            
            id_rec = json_output["ID"]
            position = json_output["Position"]
            rap_symbole = json_output["RAP-DB Gene Symbol Synonym(s)"]
            cgsnl_name = json_output["CGSNL Gene Name"]
            ory_gene_symbole = json_output["Oryzabase Gene Symbol Synonym(s)"]
            description = json_output["Description"]
            rap_name = json_output["RAP-DB Gene Name Synonym(s)"]
            ory_gene_name = json_output["Oryzabase Gene Name Synonym(s)"]
            cgsnl_gene = json_output["CGSNL Gene Symbol"]
            
            position <- as.character(position) 
            pos1 <- strsplit(position, ":")
            pos2 <- pos1[[1]][[2]]
            pos3 <- strsplit(pos2,"[..]")
            
            positiondata <- data.frame(ch=c(pos1[[1]][[1]]),
                                       st=c(pos3[[1]][[1]]),
                                       end=c(pos3[[1]][[3]]))
            

            #Permet de savoir si le gène existe deja dans notre liste
            #On ajoute seulement les gènes qui ne sont pas dans la liste
            
            if (!existsGene(liste_genes,as.character(id_rec))) {
                newgene <- GeneDB1(as.character(id_rec),
                                   locusListe[i,],
                                   as.character(rap_name),
                                   as.character(rap_symbole),
                                   as.character(cgsnl_name),
                                   as.character(cgsnl_gene),
                                   as.character(ory_gene_name),
                                   as.character(ory_gene_symbole),
                                   positiondata,
                                   as.character(description))
                #print(id_rec)
                liste_genes <- append(liste_genes,newgene)
            }
        }
    }
    return (liste_genes)
}

#' callDB3
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB3. All these locus will be stocked in liste_genes
#' @param Locus list
#' @keywords 
#' @export
#' @examples
#' callDB3(Liste de locus)
callDB3 <- function (locusListe) {
    
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    path2script = paste(c(debut,path), collapse = '')
    
    liste_genes <- data.frame()
    
    for (i in 1 : nrow(locusListe)) {
        ch = as.character(locusListe[i,1])
        start = as.character(locusListe[i,2])
        end = as.character(locusListe[i,3])
        
        #appel du script python run.py avec les attributs (chx, start, end, DB) 
        #-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "3")
        allArgs = c(path2script, args)
        Routput = system2(command, args=allArgs, stdout=TRUE)
        print(Routput)
        
        if (Routput != "not found") {
            json_output <- fromJSON(Routput)
            
            for (j in 1:length(json_output)){
                print(paste("numero",j," -> ",json_output[j][1]))
            }
            
            
            trait_gene_id = json_output["Trait Gene Id"]
            cgsnl_gene_symbol =json_output["CGSNL Gene Symbol"]
            gene_symbole_synonyme =json_output["Gene symbol synonym(s)"]
            cgsnl_gene_name = json_output["CGSNL Gene Name"]
            gene_name_synonyme =json_output["Gene name synonym(s)"]
            protein_name =json_output["Protein Name"]
            allele = json_output["Allele"]
            chromosome_no = json_output["Chromosome No."]
            explanation =json_output["Explanation"]
            trait_class =json_output["Trait Class"]
            rap_id =json_output["RAP ID"]
            gramene_id =json_output["Gramene ID"]
            arm =json_output["Arm"]
            locate_cm = json_output["Locate(cM)"]
            gene_ontology = json_output["Gene Ontology"]
            trait_ontology = json_output["Trait Ontology"]
            plant_ontology = json_output["Plant Ontology"]
            
            
            #Permet de savoir si le gène existe deja dans notre liste
            #On ajoute seulement les gènes qui ne sont pas dans la liste
            
            if (!existsGene(liste_genes,as.character(trait_gene_id))) {
                newgene <- GeneDB3("",
                                   locusListe[i,],
                                   as.character(trait_gene_id),
                                   as.character(cgsnl_gene_symbol),
                                   as.character(gene_symbole_synonyme),
                                   as.character(cgsnl_gene_name),
                                   as.character(gene_name_synonyme),
                                   as.character(protein_name),
                                   as.character(allele),
                                   as.character(chromosome_no),
                                   as.character(explanation),
                                   as.character(trait_class),
                                   as.character(rap_id),
                                   as.character(gramene_id),
                                   as.character(arm),
                                   as.character(locate_cm),
                                   as.character(gene_ontology),
                                   as.character(trait_ontology),
                                   as.character(plant_ontology))
                #print(id_rec)
                liste_genes <- append(liste_genes,newgene)
            }
        }
        else {
            print("not found")
        }
    }
    return (liste_genes)
}

#phase de json_output
#data <- data.frame(ch = c("1","1","1","1","1"),
#                   st = c("148907","5671734","9344261","10225320","148907"),
#                   end = c("248907","6337629","11332201","10325320","248907"))

#data <- data.frame(ch = c("1","1","1"),
#                   st = c("148907","5671734","5671734"),
#                   end = c("248907","6337629","6337629"))

#data <- data.frame(ch = c("1"),
#                   st = c("148907"),
#                   end = c("248907"))

#print(data)

#callDB1(data)

