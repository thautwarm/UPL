from wisepy2 import wise
from subprocess import call
from json import dumps
from mlfsf import mlfsf
from pathlib import Path
import glob
import sys


def mlfsc(*vargs, name="main", sigs="", o="./", be: str = ""):
    if not be:
        out_dir = o

        src_files = list(vargs)
        for i, src_file in enumerate(src_files):
            if src_file.endswith(".mlfs"):
                p = Path(src_file)
                f = str(p.with_suffix(".mlfsf"))
                mlfsf(src_file, f)
                src_files[i] = f

        sig_files = list(filter(lambda x: x, sigs.split(";")))

        json = dict(
            Compile=dict(
                sig_files=sig_files,
                src_files=src_files,
                out_dir=out_dir,
                out_lib_name=name,
            )
        )
        return call(["smlfs", dumps(json)])

    object_files = set()
    for each in map(glob.glob, vargs):
        object_files.update(each)
    if not Path(o).suffix:
        sys.exit("When codegen, the output path should end with a file extension.")
    json = dict(Assembly=dict(object_files=list(object_files), out=o, backend=be))
    return call(["smlfs", dumps(json)])


def entry():
    wise(mlfsc)()


if __name__ == "__main__":
    wise(mlfsc)()
