import locale
from subprocess import check_call, check_output, CalledProcessError


STOW_SRC = '/home/sergey/.mcf/bash/020-stow.bash'


def show_orphans():
    try:
        enc = locale.getdefaultlocale()[1]
        lst = check_output(['bash', '-c', 'source {} && '
                            'stow-show-orphans'.format(STOW_SRC)]).decode(enc)
        out = list()
        for line in lst.split('\n'):
            if line.startswith('Unstowed file: '):
                out.append(line[15:].strip())
        return out
    except CalledProcessError:
        print('Failed to show orphans')


def adopt_as(name):
    try:
        if not show_orphans():
            print('No orphans to stow')
            return
        check_call(['bash', '-c', 'source {} && '
                    'stow-adopt-as {}'.format(STOW_SRC, name)])
    except CalledProcessError:
        print('Failed to adopt orphans as \"{}\"'.format(name))
