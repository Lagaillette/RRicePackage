import requests
from bs4 import BeautifulSoup
import pandas as pd


def ic4r():

    html_page = requests.get("http://expression.ic4r.org/expression-api?term=Os01g0100700#showtable")
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
            for linkbody in link.findAll('td'):
                content.append(linkbody.contents)
    print(content)
    df = pd.DataFrame(content)

    print(content[0])

    print(df)

    ww = [['ouais', 'ok'], ['en fait non', 'en fait si']]
    dd = pd.DataFrame(ww)
    print(dd)