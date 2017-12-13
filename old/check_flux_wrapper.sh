#!/bin/bash
cwd=$(pwd)
cd ~/PycharmProjects/uva/
source bin/activate
python -W ignore check_flux.py $1
mv flux.txt $cwd/data
deactivate

