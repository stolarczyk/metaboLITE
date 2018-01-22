import cobra
import sys
import os

path_in = os.path.abspath("data/toycon.xml")
toycon = cobra.io.read_sbml_model(path_in)
lower = int(0)
upper = int(0)
toycon.reactions.get_by_id("E1").bounds = (lower, upper)
path_out = os.path.abspath("data/toycon_media1.xml")
cobra.io.write_sbml_model(toycon, path_out)
