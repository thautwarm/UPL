dotnet publish
delegato --exe MLFS --depdir ./bin/Debug/netcoreapp3.1/publish/ --packname smlfs --cmdname smlfs
pip uninstall smlfs -y
devp clean && python setup.py install
