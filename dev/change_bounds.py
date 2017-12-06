import cobra
import sys

toycon = cobra.io.read_sbml_model(sys.argv[1])
lower = int(sys.argv[3])
upper = int(sys.argv[4])
toycon.reactions.get_by_id(sys.argv[2]).bounds = (lower, upper)

flux = toycon.optimize().f

with open('flux_bounds.txt', 'w') as f:
    f.write('%.2f\n' % flux)
