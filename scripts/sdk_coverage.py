import sys
import io
import subprocess
import os

from header import *

def _bold(x):
    return '\x1b[1m' + x + '\033[0m'

class Coverage:
    def __init__(self, name, path, ext='**'):
        self.path = path
        self.ext = ext
        self.name = name
        self.reset()
        
    def reset(self):
        self.total = 0
        self.missing = []
        
    def check_function(self, f: Function):
        self.total += 1
        proc = subprocess.run(["rg", "--quiet", '--glob', self.ext, f.name, self.path])
        if proc.returncode != 0:
            self.missing.append(f)
            return False
        return True
        
    def check(self, header):
        for f in header:
            self.check_function(f)
            
    def report(self):
        nmissing = len(self.missing)
        found = self.total - nmissing
        print(f"{_bold(self.name)}\t{found/self.total*100:.2f}%\t({found}/{self.total})")
        for i, missing in enumerate(self.missing):
            print(f"\t{i+1}. {missing.name}")
            
class Lang:
    def __init__(self, name, ext, path=None):
        self.name = name
        self.ext = f"*.{ext}"
        self.path = path
        if self.path is None:
            self.path = self.name


if __name__ == '__main__':
    # Check for missing SDK functions
    sdks = [
        Lang('go', 'go', path='..'),
        Lang('haskell', 'hs'),
        Lang('node', 'js'),
        Lang('ocaml', 'ml'),
        Lang('php', 'php'),
        Lang('python', 'py'),
        Lang('ruby', 'rb'),
        Lang('rust', 'rs'),
    ]

    header = Header()
    for sdk in sdks:
        coverage = Coverage(sdk.name, sdk.path, sdk.ext)
        coverage.check(header)
        print()
        coverage.report()