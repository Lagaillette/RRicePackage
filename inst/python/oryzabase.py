#!/usr/bin/env python3

import helper
import requests
from bs4 import BeautifulSoup
import sys

def oryzabase(RAPID):

    link = "https://shigen.nig.ac.jp/rice/oryzabase/gene/advanced/list"

    data = {'rapId': 'Os07g0586200'}

    html_page = helper.connectionErrorPost(link, data)
    soup = BeautifulSoup(html_page.content, "lxml")

    # Headers declaration
    headers = []
    headers.append("CGSNL Gene Symbol")
    headers.append("Gene symbol synonym(s)")
    headers.append("CGSNL Gene Name")
    headers.append("Gene name synonym(s)")
    headers.append("Chr. No.")
    headers.append("Trait Class")
    headers.append("Gene Ontology")
    headers.append("Trait Ontology")
    headers.append("Plant Ontology")
    headers.append("RAP ID")
    headers.append("Mutant Image")

    dict = {}
    for search in soup.findAll('table', {"class": "table_summery_list table_nowrapTh max_width_element"}):
        i = 0
        for datafound in search.findAll('td'):
            dataFormat = datafound.text.replace('\r', '')
            dataFormat = dataFormat.replace('\n', '')
            dataFormat = dataFormat.replace('\t', '')
            dict[headers[i]] = dataFormat
            i = i + 1

    return dict

