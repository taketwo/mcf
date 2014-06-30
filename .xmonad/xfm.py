#!/usr/bin/env python3
# encoding: utf-8

import re
import os
try:
    from urllib.request import urlopen
except ImportError:
    from urllib2 import urlopen
from bs4 import BeautifulSoup


def _parse_li(li):
    raw = li.find('p', attrs={'class': 'track'}).text.strip()
    return re.match(r'(.*)[ \t\n]+(.*)', raw).groups()


if __name__ == '__main__':
    try:
        page = urlopen('http://www.xfm.co.uk/london/playlist/')
    except:
        os.exit('[xfm: failed to fetch]')
    soup = BeautifulSoup(page)
    live = True
    li = soup.body.find('li', attrs={'class': 'first nowplaying odd'})
    if li is None:
        li = soup.body.find('li', attrs={'class': 'first odd'})
        live = False
        if li is None:
            os.exit('[xfm: failed to parse]')

    print(' - '.join(_parse_li(li)))
