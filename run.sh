#!/bin/bash

# save path to return to
#cwd=$(pwd)

cd ./code/python/nnesycef

python3 ./setup.py build_ext --inplace

python3 UnifiedAgent.py
#cd $(cwd)
