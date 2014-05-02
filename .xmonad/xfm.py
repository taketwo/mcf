#!/usr/bin/env python
# encoding: utf-8

import os
import re
import urllib2
from bs4 import BeautifulSoup


def _parse_li(li):
    raw = li.find('p', attrs={'class': 'track'}).text.strip()
    return re.match(r'(.*)[ \t\n]+(.*)', raw).groups()


if __name__ == '__main__':
    page = urllib2.urlopen('http://www.xfm.co.uk/london/playlist/')
    soup = BeautifulSoup(page)
    live = True
    li = soup.body.find('li', attrs={'class': 'first nowplaying odd'})
    if li is None:
        li = soup.body.find('li', attrs={'class': 'first odd'})
        live = False
        if li is None:
            os.exit('[failed]')

    print ' - '.join(_parse_li(li))
