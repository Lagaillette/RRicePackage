#!/usr/bin/env python3
import helper
import requests
from bs4 import BeautifulSoup
import pandas as pd



def funricegenes(ID):

    link = "https://funricegenes.github.io/geneInfo.table.txt"

    """
    # Give the entire name of the file with the extension .gz
    filename = link.split("/")[-1]

    # Fetch the file by the url and decompress it
    r = requests.get(link)
    pathToFile = helper.formatPathToFile(filename)
    print(r.content)
    #if file exist
    if(not helper.existFile(pathToFile)):
        # Create the file .txt
        with open(pathToFile, "wb") as f:
            f.write(r.content)
            f.close()
    """


    # Import file tab-delimited direclty by the link
    try:
        array = pd.read_csv(link, sep="\t", header=None)
    except pd.io.common.EmptyDataError:
        array = pd.DataFrame()

    # Named columns
    array.columns = ["Symbol", "RAPdb", "MSU"]
    if (ID[:3] == "LOC"):
        data = array.loc[array['MSU'] == ID]

    else:
        data = array.loc[array['RAPdb'] == ID]

    if(data["Symbol"].empty):
        return "None"

    else:
        return data["Symbol"]


def funricegenes2(ID):

    link = "https://funricegenes.github.io/famInfo.table.txt"

    # Import file tab-delimited direclty by the link
    try:
        array = pd.read_csv(link, sep="\t", header=None)
    except pd.io.common.EmptyDataError:
        array = pd.DataFrame()
    # Named columns
    array.columns = ["Symbol", "RAPdb", "MSU", "Name"]
    if (ID[:3] == "LOC"):
        data = array.loc[array['MSU'] == ID]

    else:
        data = array.loc[array['RAPdb'] == ID]

    if(data["Symbol"].empty):
        if(data["Name"].empty):
            hashmap = {"Symbol": "None", "Name": "None"}
        else:
            hashmap = {"Symbol": "None", "Name": data["Name"].values}

    else:
        if (data["Name"].empty):
            hashmap = {"Symbol": data["Symbol"].values, "Name": "None"}
        else:
            hashmap = {"Symbol" : data["Symbol"].values, "Name" : data["Name"].values}

    return hashmap



def funricegenes3(ID):

    link = "https://funricegenes.github.io/geneKeyword.table.txt"

    # Import file tab-delimited direclty by the link
    try:
        array = pd.read_csv(link, sep="\t", header=None, encoding="utf-8")
    except pd.io.common.EmptyDataError:
        array = pd.DataFrame()
    # Named columns
    array.columns = ["Symbol", "RAPdb", "MSU", "Keyword", "Title"]
    if (ID[:3] == "LOC"):
        data = array.loc[array['MSU'] == ID]

    else:
        data = array.loc[array['RAPdb'] == ID]

    hashmap = {"Symbol" :
                   data["Symbol"].values, "Keyword" : data["Keyword"].values, "Title" : data["Title"].values}

    return hashmap

