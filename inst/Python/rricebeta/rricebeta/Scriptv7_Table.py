import requests
from bs4 import BeautifulSoup
import pandas as pd
import os
import sys

# declaration des parametres au tout debut du main

def formatPathToFile(nameFile):
    # on supprime le dernier char tant qu'on n'a pas rencontré '/'
    pathToFile = os.path.dirname(__file__)
    while not (pathToFile.endswith('/')):
        pathToFile = pathToFile[0:-1]

    pathToFile += 'resources/'+nameFile
    return pathToFile

def loadFileURL(nameFile, url):

    """
    Download the file located in the rapdb download page

    """

    # Fetch the file by the url and decompress it
    r = requests.get(url)

    # Create the file .txt
    with open(nameFile, "wb") as f:
        f.write(r.content)
        print("File created")
        f.close()


def existFile(pathToFile):
    """

    :return: return True if the file already exist, else return False
    :rtype: Bool
    """
    return (os.path.isfile(pathToFile))

def main():
    #Parameters
    # RAPID_valide = "Os06g0654600"
    RAPID = sys.argv[1]
    #End parameters

    html_page = requests.get(
        "http://rapdb.dna.affrc.go.jp/tools/search/run?keyword=" + RAPID + "&submit=Search&id=on&size=10")
    soup = BeautifulSoup(html_page.content, "lxml")
    result = soup.find('tr', attrs={"class": "result"})
    hashmap = {}
    try:
        rapid = result.find('td', attrs={"class": "c01"}).a.contents[0]
    except:
        print("Error : empty ID")
        rapid = RAPID
    try:
        description = result.find('td', attrs={"class": "c02"}).contents[0]
    except:
        print("Error : empty description")
        description = ""
    try:
        position = result.find('td', attrs={"class": "c03"}).a.contents[0]
    except:
        print("Error : empty position")
        position = ""
    try:
        RAP_symbol = result.find('td', attrs={"class": "c04"}).contents[0]
    except:
        print("Error : empty RAP symbol")
        RAP_symbol = ""
    try:
        RAP_name = result.find('td', attrs={"class": "c05"}).contents[0]
    except:
        print("Empty error")
        RAP_name = ""
    try:
        CGSNL_symbol = result.find('td', attrs={"class": "c06"}).contents[0]
    except:
        print("Empty error")
        CGSNL_symbol = ""
    try:
        CGSNL_name = result.find('td', attrs={"class": "c07"}).contents[0]
    except:
        print("Empty error")
        CGSNL_name = ""
    try:
        Oryzabase_symbol = result.find('td', attrs={"class": "c08"}).contents[0]
    except:
        print("Empty error")
        Oryzabase_symbol = ""
    try:
        Oryzabase_name = result.find('td', attrs={"class": "c09"}).contents[0]
    except:
        print("Empty error")
        Oryzabase_name = ""

    hashmap = {"ID": rapid,
               "Description": description,
               "Position": position,
               "RAP-DB Gene Symbol Synonym(s)": RAP_symbol,
               "RAP-DB Gene Name Synonym(s)": RAP_name,
               "CGSNL Gene Symbol": CGSNL_symbol,
               "CGSNL Gene Name": CGSNL_name,
               "Oryzabase Gene Symbol Synonym(s)": Oryzabase_symbol,
               "Oryzabase Gene Name Synonym(s)": Oryzabase_name
               }

    print(hashmap['CGSNL Gene Name'])
    print(len(hashmap))
    pathToFile = '../resources/OryzabaseGeneListEn.txt'
    if (existFile(pathToFile) == False):
        loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
    else:
        print("File already exist")
    print("Find file OK")
    #self.oryzabase(hashmap)
    print(hashmap)
    return hashmap


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()


