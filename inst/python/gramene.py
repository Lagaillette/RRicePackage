#!/usr/bin/env python3

import helper

def gramene(RAPID):

        """
        Download the file located in the URL

        :param url: The url for accessing the file
        :type url: String
        """

        # Fetch the file by the url and decompress it
        link = 'http://data.gramene.org/v53/genes?q=' + RAPID + '&bedFeature=gene&bedCombiner=canonical'
        html_page = helper.connectionError(link)
        return html_page.content.decode('UTF-8')