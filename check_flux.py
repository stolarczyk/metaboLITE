import cobra
import sys

toycon = cobra.io.read_sbml_model(sys.argv[1])
flux = toycon.optimize().f

with open('flux.txt', 'w') as f:
    f.write('%.2f' % flux)
