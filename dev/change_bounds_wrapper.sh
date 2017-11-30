#!/bin/bash
cwd=$(pwd)
cd ~/PycharmProjects/uva/
source bin/activate
python -W ignore change_bounds.py $1 $2
mv flux_bounds.txt $cwd/data
deactivate

