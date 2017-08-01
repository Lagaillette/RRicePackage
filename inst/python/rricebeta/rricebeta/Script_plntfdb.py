#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip



def plntfdb(ID):

    html_page = requests.get('http://plntfdb.bio.uni-potsdam.de/v3.0/get_id.php?seq_id='+ID)
    soup = BeautifulSoup(html_page.content, "lxml")
    # Find headers
    for search in soup.findAll('div', { "id" : "subcontent" }):
       i=1
       for linkfound in search.findAll('a'):
           # third line
           if(i==3):
               if(linkfound.contents[0]):
                   return linkfound.contents[0]

               else:
                   return False
           i = i + 1

