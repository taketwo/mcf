#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import time
import signal
import logging
import imaplib
import argparse
from pathlib import Path
from contextlib import contextmanager
from systemd.journal import JournalHandler


log = logging.getLogger('gmail')
log.addHandler(JournalHandler())
log.setLevel(logging.INFO)


class CredentialsError(Exception):
    pass


class TimeoutError(Exception):
    pass


@contextmanager
def timeout(seconds):
    def signal_handler(signum, frame):
        raise TimeoutError
    signal.signal(signal.SIGALRM, signal_handler)
    signal.alarm(seconds)
    try:
        yield
    finally:
        signal.alarm(0)


class Gmail(object):

    REGEX = re.compile(r'X-GM-THRID (\d+) UID')

    def __init__(self, user, token):
        self.imap = imaplib.IMAP4_SSL('imap.gmail.com', '993')
        self.imap.login('{}@gmail.com'.format(user), token)
        self.imap.select()
        log.info('Established connection with GMail IMAP server')

    def close(self):
        self.imap.close()
        self.imap.logout()
        log.info('Closed connection with GMail IMAP server')

    def get_unread_count(self):
        """
        Get the number of unread messages.
        Throws if connection failed.
        """
        self.imap.select()
        uids = self.imap.uid('search', None, 'UnSeen')[1][0].decode().split()
        if not uids:
            return 0
        query = ','.join(uids).encode('utf-8')
        items = [t.decode() for t in self.imap.uid('fetch', query, '(X-GM-THRID)')[1]]
        tids = self.REGEX.findall(' '.join(items))
        return len(set(tids))

    def get_unread_count_every(self, interval):
        """
        Check and yield the number of unread messages at given intervals.
        Throws if connection timed out or otherwise failed.
        """
        while True:
            with timeout(args.interval):
                yield self.get_unread_count()
            time.sleep(args.interval)


def load_credentials(path):
    with open(str(path), 'r') as f:
        user = f.readline().strip()
        token = f.readline().strip()
    if not user or not token:
        raise CredentialsError('Loaded user name or token is empty')
    return user, token


def make_indicator(unread_count):
    if unread_count:
        return '%{{F#fdf6e3 B#cb4b16}}  {} %{{F- B-}}'.format(unread_count)
    return ''


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Gmail unread count.
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-i', '--interval', type=int, default=10,
                        help='check interval (seconds)')
    args = parser.parse_args()

    try:
        path = Path(os.path.dirname(os.path.realpath(__file__))) / 'gmail.token'
        user, token = load_credentials(path)
    except Exception as e:
        log.critical(e)
        sys.exit()

    while True:
        try:
            gmail = Gmail(user, token)
            for unread in gmail.get_unread_count_every(args.interval):
                print(make_indicator(unread), flush=True)
        except TimeoutError:
            log.warn('Communication timeout, will wait and reconnect')
        except Exception as e:
            log.warn('Exception: {}'.format(e))
        finally:
            gmail.close()
        time.sleep(args.interval)
