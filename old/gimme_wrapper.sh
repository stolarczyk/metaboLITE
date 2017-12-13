#!/bin/bash
cwd=$(pwd)
cd ~/PycharmProjects/uva_oldcobra/
source bin/activate
python -W ignore runGIMME.py $1
mv fluxes_gimme.csv $cwd/data
deactivate

