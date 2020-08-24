def exe():
    from pathlib import Path
    from subprocess import call
    import sys

    cmd = str((Path(__file__).parent / "publish/MLFS").absolute())
    call([cmd, *sys.argv[1:]])
