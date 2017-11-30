import framed
import random
import csv
import sys

model = framed.load_cbmodel(sys.argv[1], flavor='fbc2')
expr = {}
for g in model.genes.keys():
    expr[g] = random.randint(10, 100)

result = framed.GIMME(model=model, gene_exp=expr)
fluxes = dict(result.values)
with open('gimme_fluxes.csv', 'wb') as f:
    w = csv.writer(f)
    w.writerows(fluxes.items())
