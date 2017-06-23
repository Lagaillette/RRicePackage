import requests
from bs4 import BeautifulSoup
import pandas as pd
import os
import sys

# declaration des parametres au tout debut du main


def existFile(pathToFile):
    """

    :return: return True if the file already exist, else return False
    :rtype: Bool
    """
    return (os.path.isfile(pathToFile))



def downloadFiles(hashmap):

    """
    Download files contained in the hashmap[nameFile] : URL

    """

    # key is the name of the file and the value is the URL
    for key, value in hashmap.items():

        pathToFile = '../resources/'+key

        #If the file exists in the folder resources
        if(existFile(pathToFile) == True):
            print("File "+key+" already exist")

        #If the file doesn't exist
        else:
            # Fetch the file by the url and decompress it
            r = requests.get(value)

            # Create the file .txt
            with open(pathToFile, "wb") as f:
                f.write(r.content)
                print(pathToFile+" created")
                f.close()


def splitPath():
    # on supprime le dernier char tant qu'on n'a pas rencontré '/'
    pathToFile = os.path.dirname(__file__)
    while not (pathToFile.endswith('/')):
        pathToFile = pathToFile[0:-1]

    pathToFile += 'resources/OryzabaseGeneListEn.txt'
    print(pathToFile)



def main():
    #Parameters
    chr = sys.argv[1]
    #End parameters

    if(int(chr) < 10):
        chrString = "0"+chr

    html_page = requests.get("http://rice.plantbiology.msu.edu/pub/data/Eukaryotic_Projects/o_sativa/annotation_dbs/pseudomolecules/version_7.0/chr"+chrString+".dir/")
    soup = BeautifulSoup(html_page.content, "lxml")
    linksList = {}
    for link in soup.findAll('a'):
        linkname = link.get('href')
        # If the first letter is a C, it places the link into the list
        if(linkname[0] == "C"):
            linkfound = "http://rice.plantbiology.msu.edu/pub/data/Eukaryotic_Projects/o_sativa/annotation_dbs/pseudomolecules/version_7.0/chr01.dir/" + linkname
            linksList[linkname] = linkfound
    downloadFiles(linksList)
    splitPath()


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()