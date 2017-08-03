#!/usr/bin/env python3

import helper
from bs4 import BeautifulSoup
import pandas as pd


def ic4r(RAPID):

    link = 'http://expression.ic4r.org/expression-api?term='+RAPID+'#showtable'
    html_page = helper.connectionError(link)
    soup = BeautifulSoup(html_page.content, "lxml")
    # Find headers
    headers = []
    for head in soup.findAll('thead'):
        for link in head.findAll('tr'):
            for linkhead in link.findAll('th'):
                headers.append(linkhead.contents)
    content = []
    for body in soup.findAll('tbody'):
        for link in body.findAll('tr'):
            dict = {}
            i=0
            for linkbody in link.findAll('td'):
                dict[str(headers[i][0])] = linkbody.contents
                i = i+1

            #content.append(dict)
            print(dict)
    """
    df = pd.DataFrame(content)
    # Affichage premiere ligne
    #print(df.loc[0])
    return df.to_json()
    """
