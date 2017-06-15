import requests
from bs4 import BeautifulSoup
import pandas as pd


class ScriptV7():

    def __init__(self):
        self.id = ""
        self.description = ""
        self.position = ""
        self.RAP_symbol = ""
        self.RAP_name = ""
        self.CGSNL_symbol = ""
        self.CGSNL_name = ""
        self.Oryzabase_symbol = ""
        self.Oryzabase_name = ""

    # It's working
    def rapToLoc(self, RAPID):

        html_page = requests.get("http://rapdb.dna.affrc.go.jp/tools/search/run?keyword="+RAPID+"&submit=Search&id=on&size=10")
        soup = BeautifulSoup(html_page.content, "lxml")
        result = soup.find('tr', attrs={"class": "result"})

        self.id = result.find('td', attrs={"class": "c01"}).a.contents[0]
        self.description = result.find('td', attrs={"class": "c02"}).contents[0]
        self.position = result.find('td', attrs={"class": "c03"}).a.contents[0]
        self.RAP_symbol = result.find('td', attrs={"class": "c04"}).contents
        self.RAP_name = result.find('td', attrs={"class": "c05"}).contents
        self.CGSNL_symbol = result.find('td', attrs={"class": "c06"}).contents[0]
        self.CGSNL_name = result.find('td', attrs={"class": "c07"}).contents[0]
        self.Oryzabase_symbol = result.find('td', attrs={"class": "c08"}).contents[0]
        self.Oryzabase_name = result.find('td', attrs={"class": "c09"}).contents[0]


        hashmap = {"ID" : self.id,
                   "Description" : self.description,
                   "Position" : self.position,
                   "RAP-DB Gene Symbol Synonym(s)" : self.RAP_symbol,
                   "RAP-DB Gene Name Synonym(s)" : self.RAP_name,
                   "CGSNL Gene Symbol" : self.CGSNL_symbol,
                   "CGSNL Gene Name" : self.CGSNL_name,
                   "Oryzabase Gene Symbol Synonym(s)" : self.Oryzabase_symbol,
                   "Oryzabase Gene Name Synonym(s)" : self.Oryzabase_name
                   }

        print(hashmap['CGSNL Gene Name'])
        print(len(hashmap))
        self.oryzabase(hashmap['CGSNL Gene Name'])
        return hashmap





    def oryzabase(self, CG_name):
        # Import file tab-delimited
        try:
            array = pd.read_csv("../OryzabaseGeneListEn_20170615010051.txt", sep="\t", encoding='utf-8')
            print("ok")
            #array = pd.read_csv(self.file, sep="\t", names=['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology'])

        except NameError:
            array = pd.DataFrame()

        #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']
        data = array.loc[array['CGSNL Gene Name'] == CG_name]
        return data