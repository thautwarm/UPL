from wisepy2 import wise
from subprocess import call
from json import dumps
from mlfsf import mlfsf
from pathlib import Path

def mlfsc(*vargs, name="main", sigs="", o="./"):
    out_dir = o

    src_files = list(vargs)
    for i, src_file in enumerate(src_files):
        if src_file.endswith(".mlfs"):
            p = Path(src_file)
            f = str(p.with_suffix('.mlfsf'))
            mlfsf(src_file, f)
            src_files[i] = f

    sig_files = list(map(dumps, filter(lambda x: x, sigs.split(";"))))

    json = dict(
        sig_files=sig_files,
        src_files=src_files,
        out_dir=out_dir,
        out_lib_name=name)
    call(['smlfs', dumps(json)])

def entry():
    wise(mlfsc)()

if __name__ == "__main__":
    wise(mlfsc)()