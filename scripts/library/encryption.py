from os import remove
from os.path import isfile
from subprocess import check_call, CalledProcessError

# subprocess.DEVNULL was introduced in 3.3, therefore not available on
# contemporary Ubuntu systems
try:
    from subprocess import DEVNULL
except:
    DEVNULL = None

OPENSSL_CMD = 'openssl aes-256-cbc -a {opt} -in {src} -out {tgt}'


def decrypt(src, tgt=None, passwd=None):
    tgt = src + '.dec' if not tgt else tgt
    cmd = OPENSSL_CMD.format(src=src, tgt=tgt, opt='-d').split()
    if passwd:
        cmd += ['-k', passwd]
    try:
        check_call(cmd, stderr=DEVNULL)
        return True
    except CalledProcessError:
        if isfile(tgt):
            remove(tgt)
        return False


def encrypt(src, tgt=None, passwd=None):
    tgt = src + '.enc' if not tgt else tgt
    cmd = OPENSSL_CMD.format(src=src, tgt=tgt, opt='-salt').split()
    if passwd:
        cmd += ['-k', passwd]
    try:
        check_call(cmd, stderr=DEVNULL)
        return True
    except CalledProcessError:
        if isfile(tgt):
            remove(tgt)
        return False
