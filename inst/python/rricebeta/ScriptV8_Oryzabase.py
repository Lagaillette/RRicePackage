#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import pandas as pd
import os

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

def oryzabaseCGSNL(CGSNL):
    #pathToFile = '../resources/OryzabaseGeneListEn.txt'
    pathToFile = formatPathToFile('OryzabaseGeneListEn.txt')
    if(existFile(pathToFile) == False):
        loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
    else:
        print("File already exist")
    print("Find file OK")

    # Import file tab-
    nameFile = formatPathToFile("OryzabaseGeneListEn.txt")
    try:
        array = pd.read_csv(nameFile, sep="\t", encoding='utf-8')
        #array = pd.read_csv(self.file, sep="\t", names=['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology'])

    except NameError:
        array = pd.DataFrame()

    print(array)

    #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']
    print("Find by CGSNL Gene Name")
    data = array.loc[array['CGSNL Gene Name'] == CGSNL]
    return data

def oryzabaseRapId(RAPID):
    #pathToFile = '../resources/OryzabaseGeneListEn.txt'
    pathToFile = formatPathToFile('OryzabaseGeneListEn.txt')
    if(existFile(pathToFile) == False):
        loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
    else:
        print("File already exist")
    print("Find file OK")

    # Import file tab-delimited
    nameFile = formatPathToFile("OryzabaseGeneListEn.txt")
    try:
        array = pd.read_csv(nameFile, sep="\t", encoding='utf-8')
        #array = pd.read_csv(self.file, sep="\t", names=['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology'])

    except NameError:
        array = pd.DataFrame()

    #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']

    data = array.loc[array['RAP ID'] == RAPID]

    print(data)
    return data

