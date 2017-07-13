#!/usr/bin/env python3

import requests

def gramene(RAPID):

        """
        Download the file located in the URL

        :param url: The url for accessing the file
        :type url: String
        """

        # Fetch the file by the url and decompress it
        r = requests.get('http://data.gramene.org/v53/genes?q=' + RAPID + '&bedFeature=gene&bedCombiner=canonical')
        return r.content.decode('UTF-8')