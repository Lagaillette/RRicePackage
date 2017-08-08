#!/usr/bin/env python3

import helper
import requests
from bs4 import BeautifulSoup
import pandas as pd
import gzip



def planttfdb(MSUID):

    # Find the file
    url = 'http://planttfdb.cbi.pku.edu.cn/download.php'
    html_page = helper.connectionError(url)
    soup = BeautifulSoup(html_page.content, "lxml")
    # Find headers
    for search in soup.findAll('table', { "id" : "oid_tfid" }):
       for linkfound in search.findAll('a'):
           if (linkfound.contents[0] == "Oryza sativa subsp. japonica"):
               link = 'http://planttfdb.cbi.pku.edu.cn/'+linkfound.get('href')
               break

    # Give the entire name of the file with the extension .gz
    filename = link.split("/")[-1]

    # Give the name of the file without .gz
    uncompressName = filename[:-3] + ".txt"
    pathToFile = helper.formatPathToFile(uncompressName)

    # Test existant file
    if(not helper.existFile(pathToFile)):
        # Fetch the file by the url and decompress it
        r = requests.get(link)
        decompressedFile = gzip.decompress(r.content)

        # Create the file .txt



        with open(pathToFile, "wb") as f:
                f.write(decompressedFile)
                f.close()

    # Use the previous created file (.txt)
    with open(pathToFile, "r+b") as file:

        # Import file tab-delimited

        try:
            array = pd.read_csv(file, sep="\t", header=None)
        except pd.io.common.EmptyError:
                array = pd.DataFrame()
        # Named columns
        array.columns = ["TF_ID", "Gene_ID", "Family"]

        data = array.loc[array['TF_ID'] == MSUID]

    if (not data.empty):

        return data
    else:
        data = array.loc[array['Gene_ID'] == MSUID]

    if (data.empty):

        return False
    else:

        hashmap = {"Family": data["Family"].values[0]}
        return hashmap

