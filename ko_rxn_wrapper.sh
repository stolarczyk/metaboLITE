#!/bin/bash
cwd=$(pwd)
cd ~/PycharmProjects/uva/
source bin/activate
python -W ignore ko_rxn.py $1 $2
mv flux_ko.txt $cwd/data
deactivate

