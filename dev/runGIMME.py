import framed
import random
import csv
import sys

model = framed.load_cbmodel(sys.argv[1], flavor='fbc2')
expr = {}
for g in model.genes.keys():
    expr[g] = random.randint(-100, 100)
growth = random.random()

result = framed.GIMME(model=model, gene_exp=expr, growth_frac=growth,cutoff=90)
fluxes = dict(result.values)
with open('fluxes_gimme.csv', 'wb') as f:
    w = csv.writer(f)
    w.writerows(fluxes.items())
