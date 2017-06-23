# Imports
import os
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip
import requests
import ijson
from pprint import pprint
import json



class ScriptV4:

    #constructor
    def __init__(self):
        self.url = ""
        self.nameFile = ""
        self.pathToFile = "../resources/"+self.nameFile

    def loadFileURL(self, url="http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"):

        """
        Download the file located in the URL

        :param url: The url for accessing the file
        :type url: String
        """

        self.url = url

        # Fetch the file by the url and decompress it
        r = requests.get(self.url)

        # Create the file .txt
        with open(self.pathToFile, "wb") as f:
            f.write(r.content)
            f.close()


    def existFile(self):
        """

        :return: return True if the file already exist, else return False
        :rtype: Bool
        """
        return (os.path.isfile(self.pathToFile))


    def rapToLoc(self, RAPID):

        """

        :param RAPID: the RAP ID that you want LOC ID
        :type RAPID:
        """

        self.nameFile = "fileJSON.json"
        self.pathToFile = "../resources/" + self.nameFile

        # If the file doesn't exist, it download it
        if(self.existFile() == False):
            self.loadFileURL('http://data.gramene.org/v53/genes?q='+RAPID+'&bedFeature=gene&bedCombiner=canonical')
            print("File created")
            print(self.pathToFile)
        else:
            print("File already exist")
        with open(self.nameFile, 'r') as f:
            data = json.load(f)
            i=0
            hashmap={}
            while(data[0]["xrefs"][i]["db"] != "TIGR_LOCUS"):
                i += 1
            hashmap["LOCID"] = data[0]["xrefs"][i]["ids"]
            f.close()
        return hashmap


    def xrefs(self, RAPID):

        self.nameFile = "fileJSON.json"
        self.pathToFile = "../resources/"+self.nameFile

        # If the file doesn't exist, it download it
        if (self.existFile() == False):
            self.loadFileURL('http://data.gramene.org/v53/genes?q=' + RAPID + '&bedFeature=gene&bedCombiner=canonical')
            print("File created")
            print(self.pathToFile)
        else:
            print("File already exist")
        with open(self.pathToFile, 'r') as f:
            data = json.load(f)
            i = 0
            hashmap =  {}
            while (i < len(data[0]["xrefs"])):

                if (data[0]["xrefs"][i]["db"] == "Uniprot/SPTREMBL"):
                    hashmap[data[0]["xrefs"][i]["db"]] = data[0]["xrefs"][i]["ids"]
                    #hashmap["Uniprot/SPTREMBL"] = data[0]["xrefs"][i]["ids"]
                
                elif (data[0]["xrefs"][i]["db"] == "RefSeq_peptide"):
                    hashmap["RefSeq_peptide"] = data[0]["xrefs"][i]["ids"]

                elif (data[0]["xrefs"][i]["db"] == "RefSeq_mRNA"):
                    hashmap["RefSeq_mRNA"] = data[0]["xrefs"][i]["ids"]

                elif (data[0]["xrefs"][i]["db"] == "EntrezGene"):
                    hashmap["EntrezGene"] = data[0]["xrefs"][i]["ids"]

                i += 1

            f.close()
        print(hashmap)
        return hashmap

    def taxonomy(self, taxonId):
        self.nameFile = "fileJSON.json"

        # If the file doesn't exist, it download it
        if (self.existFile() == False):
            self.loadFileURL('http://data.gramene.org/v53/genes?bedFeature=gene&bedCombiner=canonical&taxon_id='+taxonId)
            print("File created")
            print(self.pathToFile)
        else:
            print("File already exist")
        with open(self.nameFile, 'r') as f:
            data = json.load(f)
            hashmap = {}
            hashmap["id"] = data[0]["annotations"]["taxonomy"]["entries"][0]["_id"]
            hashmap["name"] = data[0]["annotations"]["taxonomy"]["entries"][0]["name"]
            f.close()
        return hashmap




