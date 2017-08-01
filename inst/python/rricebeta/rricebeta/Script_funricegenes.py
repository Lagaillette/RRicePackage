#!/usr/bin/env python3
import helper
import requests
from bs4 import BeautifulSoup
import pandas as pd
from pandas.io.common import EmptyDataError



def funricegenes(ID):

    link = "https://funricegenes.github.io/geneInfo.table.txt"

    # Give the entire name of the file with the extension .gz
    filename = link.split("/")[-1]

    # Fetch the file by the url and decompress it
    r = requests.get(link)
    pathToFile = helper.formatPathToFile(filename)

    #if file exist
    if(not helper.existFile(pathToFile)):
        # Create the file .txt
        with open(pathToFile, "wb") as f:
            f.write(r.content)
            f.close()

    # Use the previous file (.txt)
    with open(pathToFile, "r+b") as file:

        # Import file tab-delimited
        try:
            array = pd.read_csv(file, sep="\t", header=None)
        except EmptyDataError:
            array = pd.DataFrame()
        # Named columns
        array.columns = ["Symbol", "RAPdb", "MSU"]

        if (ID[:2] == "LOC"):
            data = array.loc[array['MSU'] == ID]

        else:
            data = array.loc[array['RAPdb'] == ID]

        print(data)