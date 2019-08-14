from os import remove
from os.path import isfile
from subprocess import check_call, check_output, CalledProcessError

# subprocess.DEVNULL was introduced in 3.3, therefore not available on
# contemporary Ubuntu systems
try:
    from subprocess import DEVNULL
except ImportError:
    DEVNULL = None

OPENSSL_CMD = "openssl enc -aes-256-cbc -md sha512 -pbkdf2 -iter 1000 {opt} -in {src}"


def decrypt(src, tgt=None, passwd=None):
    """
    Decrypt AES256-encoded file.
    Returns decoded string if tgt is not given, writes to file otherwise.
    """
    cmd = OPENSSL_CMD.format(src=src, opt="-d").split()
    if tgt:
        cmd += ["-out", tgt]
    if passwd:
        cmd += ["-k", passwd]
    try:
        if tgt:
            check_call(cmd, stderr=DEVNULL)
            return True
        else:
            return check_output(cmd, stderr=DEVNULL).decode("ascii")
    except CalledProcessError:
        if tgt and isfile(tgt):
            remove(tgt)
        return False


def encrypt(src, tgt=None, passwd=None):
    cmd = OPENSSL_CMD.format(src=src, opt="-salt").split()
    if tgt:
        cmd += ["-out", tgt]
    if passwd:
        cmd += ["-k", passwd]
    try:
        if tgt:
            check_call(cmd, stderr=DEVNULL)
            return True
        else:
            return check_output(cmd, stderr=DEVNULL).decode("ascii")
    except CalledProcessError:
        if tgt and isfile(tgt):
            remove(tgt)
        return False
