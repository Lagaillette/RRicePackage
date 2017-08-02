#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import pandas as pd
import gzip



def planttfdb(MSUID):

    html_page = requests.get('http://planttfdb.cbi.pku.edu.cn/download.php')
    soup = BeautifulSoup(html_page.content, "lxml")
    # Find headers
    for search in soup.findAll('table', { "id" : "oid_tfid" }):
       for linkfound in search.findAll('a'):
           if (linkfound.contents[0] == "Oryza sativa subsp. japonica"):
               link = 'http://planttfdb.cbi.pku.edu.cn/'+linkfound.get('href')
               break

    # Import file tab-delimited
    try:
        array = pd.read_csv(link, sep="\t", header=None)
    except pd.io.common.EmptyDataError:
        array = pd.DataFrame()
    # Named columns
    array.columns = ["TF_ID", "Gene_ID", "Family"]

    data = array.loc[array['TF_ID'] == MSUID]
    if (not data.empty):
        return data["Family"]
    else:
        data = array.loc[array['Gene_ID'] == MSUID]
        if (data.empty):
            return False
        else:
            return data["Family"].values

