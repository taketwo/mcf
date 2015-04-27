#!/usr/bin/env python
# encoding: utf-8

import requests
import logging
logging.captureWarnings(True)


class Toggl:

    BASE_URI = 'https://www.toggl.com/api/v8/'

    def __init__(self, api_token):
        self.auth = (api_token, 'api_token')

    def get(self, action):
        return requests.get(Toggl.BASE_URI + action, auth=self.auth)

    def time_entries(self):
        return self.get('time_entries').json()

    def current(self):
        return self.get('time_entries/current').json()['data']
