from powerline_shell.utils import BasicSegment
from socket import gethostname


class Segment(BasicSegment):
    def add_to_powerline(self):
        hostname = gethostname()
        if not hostname.endswith("docker"):
            return
        powerline = self.powerline
        if powerline.args.shell == "bash":
            host_prompt = r" \h "
        elif powerline.args.shell == "zsh":
            host_prompt = " %m "
        else:
            host_prompt = " %s " % hostname.split(".")[0]
        powerline.append(host_prompt, 15, 6)
