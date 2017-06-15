import requests
from bs4 import BeautifulSoup
import pandas as pd


class ScriptV8():

    def __init__(self):
        self.file = "../OryzabaseGeneListEn_20170615010051.txt"

    def oryzabase(ScriptV7, CG_name):
        # Import file tab-delimited
        try:
            array = pd.read_csv(self.file, sep="\t", encoding='utf-8')
            print("ok")
            #array = pd.read_csv(self.file, sep="\t", names=['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology'])

        except NameError:
            array = pd.DataFrame()

        #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']
        data = array.loc[array['CGSNL Gene Name	Gene name synonym(s)'] == CG_name]
        return data