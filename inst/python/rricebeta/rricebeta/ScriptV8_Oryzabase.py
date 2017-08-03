#!/usr/bin/env python3

import helper
import requests
from bs4 import BeautifulSoup
import pandas as pd
import os

def loadFileURL(nameFile, url):

    """
    Download the file located in the rapdb download page

    """

    # Fetch the file by the url and decompress it
    html_page = helper.connectionError(url)

    # Create the file .txt
    with open(nameFile, "wb") as f:
        f.write(html_page.content)
        print("File created")
        f.close()


def oryzabaseCGSNL(CGSNL):
    pathToFile = helper.formatPathToFile("OryzabaseGeneListEn.txt")
    if(helper.existFile(pathToFile) == False):
        loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
    else:
        print("File already exist")
    print("Find file OK")

    # Import file tab-
    try:
        array = pd.read_csv(pathToFile, sep="\t", encoding='utf-8')

    except NameError:
        array = pd.DataFrame()

    print(array)

    print("Find by CGSNL Gene Name")
    data = array.loc[array['CGSNL Gene Name'] == CGSNL]
    return data

def oryzabaseRapId(RAPID):
    pathToFile = helper.formatPathToFile("OryzabaseGeneListEn.txt")
    if(helper.existFile(pathToFile) == False):
        loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
    else:
        print("File already exist")
    print("Find file OK")

    # Import file tab-delimited
    try:
        array = pd.read_csv(pathToFile, sep="\t", encoding='utf-8')

    except NameError:
        array = pd.DataFrame()

    #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']

    data = array.loc[array['RAP ID'] == RAPID]

    return data

