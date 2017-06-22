command ="python3"


#exemple RAPID = "Os06g0654600" 
RAPID = "Os06g0654600" 

#pour appeler le fichier SCriptv7_Table.py -> BESOIN de l'attribut "RAPID"
Appel_Scriptv7 <- function (RAPID) {
  
#à modifier en fonction de l'utilisateur
#path2script="/home/ioit.user4/RRicePackage/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"
path2script="RRicePackage/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"
  
# Build up args in a vector
# RAPID_valide = "Os06g0654600" -> exemple valide
args = c(RAPID)

# Add path to script as first arg
allArgs = c(path2script, args)

Routput = system2(command, args=allArgs, stdout=TRUE)

#Appelé à chaque fois qu'elle rencontrera un print
print(Routput)
}

Appel_Scriptv7(RAPID)
