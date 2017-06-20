import os
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip
import requests
import json


r = requests.get("http://data.gramene.org/v53/genes?q=Os01g0102900&bedFeature=gene&bedCombiner=canonical")
name="testJson.json"
with open(name, "wb") as f:
    f.write(r.content)
    f.close()

with open(name, 'r') as f:
    data = json.load(f)
    i = 0
    while (data[0]["xrefs"][i]["db"] != "TIGR_LOCUS"):
        i += 1
    print(data[0]["xrefs"][i]["ids"])
    f.close()