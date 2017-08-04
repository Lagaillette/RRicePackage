#!/usr/bin/env python3

import helper
import requests
from bs4 import BeautifulSoup
import os
import sys
import json

def oryzabase(RAPID):

    link = "https://shigen.nig.ac.jp/rice/oryzabase/gene/advanced/search"

    data = {'rapId': 'Os07g0586200'}
    #Fake user agent to avoid website reject
    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.0; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0'}

    r = requests.post(link, headers=headers, data=data)
    html_page = helper.connectionError(r.link)
    print(html_page.content)


