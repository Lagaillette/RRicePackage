# Imports
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip
import requests
import ijson
from pprint import pprint
import json



class RRice:

    #constructor
    def __init__(self):
        self.url = ""
        self.nameFile = ""

    def loadFileURL(self, url="http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"):

        """
        Download the file located in the URL

        :param url: The url for accessing the file
        :type url: String
        """

        self.url = url
        filename = "fileJSON.json"
        # Give the name of the file without .gz
        uncompressName = filename[:-3]

        # Fetch the file by the url and decompress it
        r = requests.get(self.url)

        # Create the file .txt
        with open(filename, "wb") as f:
            f.write(r.content)
            f.close()

        # Return the name of the created file
        self.nameFile = uncompressName
        #return uncompressName


    def rapToLoc(self, RAPID):

        self.loadFileURL('http://data.gramene.org/v53/genes?q='+RAPID+'&bedFeature=gene&bedCombiner=canonical')
        filename = "fileJSON.json"
        data = []
        with open(filename, 'r') as f:
            data = json.load(f)
            print(data[0]["xrefs"][1])
