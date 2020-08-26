from wisepy2 import wise
from subprocess import call
from json import dumps
from mlfsf import mlfsf
from pathlib import Path
import glob


def mlfsc(mode, *vargs, name="main", sigs="", o="./", cg: bool = False):
    if not cg:
        out_dir = o

        src_files = list(vargs)
        for i, src_file in enumerate(src_files):
            if src_file.endswith(".mlfs"):
                p = Path(src_file)
                f = str(p.with_suffix('.mlfsf'))
                mlfsf(src_file, f)
                src_files[i] = f

        sig_files = list(filter(lambda x: x, sigs.split(";")))

        json =
            dict(Compile=
                dict(
                    sig_files=sig_files,
                    src_files=src_files,
                    out_dir=out_dir,
                    out_lib_name=name))
        return call(['smlfs', dumps(json)])
    glob.glob("*.mlfso")


def entry():
    wise(mlfsc)()

if __name__ == "__main__":
    wise(mlfsc)()