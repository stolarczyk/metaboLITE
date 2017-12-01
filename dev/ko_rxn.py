import cobra
import sys

toycon = cobra.io.read_sbml_model(sys.argv[1])
toycon.reactions.get_by_id(sys.argv[2]).knock_out()

flux = toycon.optimize().f



path = sys.argv[1]
fragments = path.split("/")
last_fragment = fragments[-1]
fragments = fragments[:-1]
filename = last_fragment.split(".")[0]
filename = filename + "_removed_" + sys.argv[2] + ".xml"
fragments[-1] = filename
path_removed = '/'.join(fragments)

with open('flux_ko.txt', 'w') as f:
    f.write('%.2f,%s\n' % (flux, path_removed))

toycon.reactions.get_by_id(sys.argv[2]).remove_from_model()
cobra.io.write_sbml_model(toycon, path_removed)
