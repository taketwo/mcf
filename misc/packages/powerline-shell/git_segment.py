from subprocess import CalledProcessError, check_output

from powerline_shell.segments.git import Segment as GitSegment


class Segment(GitSegment):
    def run(self):
        try:
            fs = check_output("stat -f -c %T .", shell=True)
            if fs == "fuseblk":
                self.stats, self.branch = "", ""
                return
        except CalledProcessError:
            pass
        GitSegment.run(self)
