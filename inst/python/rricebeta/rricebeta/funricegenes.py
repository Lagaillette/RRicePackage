#!/usr/bin/env python3
import helper
import requests
from bs4 import BeautifulSoup
import pandas as pd



def funricegenes(ID):

    link = "https://funricegenes.github.io/geneInfo.table.txt"

    # Import file tab-delimited direclty by the link (currently no connection test)
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

    if (data["Symbol"].empty):
        hashmap = {"Symbol": "None"}
    else:
        hashmap = {"Symbol": data["Symbol"].values[0]}

    return hashmap



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
            hashmap = {"Symbol": "None", "Name": data["Name"].values[0]}

    else:
        if (data["Name"].empty):
            hashmap = {"Symbol": data["Symbol"].values[0], "Name": "None"}
        else:
            hashmap = {"Symbol" : data["Symbol"].values[0], "Name" : data["Name"].values[0]}

    return hashmap



def funricegenes3(ID):
    link = "https://funricegenes.github.io/geneKeyword.table.txt"

    # Import file tab-delimited direclty by the link
    try:
        array = pd.read_csv(link, sep="\t", header=None, encoding='latin-1')
    except pd.io.common.EmptyDataError:
        array = pd.DataFrame()
    # Named columns
    array.columns = ["Symbol", "RAPdb", "MSU", "Keyword", "Title"]
    if (ID[:3] == "LOC"):
        data = array.loc[array['MSU'] == ID]

    else:
        data = array.loc[array['RAPdb'] == ID]

    hashmap = {}
    if(data["Symbol"].empty):
        hashmap["Symbol"] = "None"
    else:
        hashmap["Symbol"] = data["Symbol"].values[0]

    if(data["Keyword"].empty):
        hashmap["Keyword"] = "None"
    else:
        hashmap["Keyword"] = data["Keyword"].values[0]

    if(data["Title"].empty):
        hashmap["Title"] = "None"
    else:
        hashmap["Title"] = data["Title"].values[0]

    return hashmap

