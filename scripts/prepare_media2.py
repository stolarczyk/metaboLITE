import cobra
import sys
import os

path_in = os.path.abspath("data/toycon.xml")
toycon = cobra.io.read_sbml_model(path_in)
lower = int(-100)
upper = int(100)
toycon.reactions.get_by_id("E2").bounds = (lower, upper)
lower = int(-100)
upper = int(100)
toycon.reactions.get_by_id("E4").bounds = (lower, upper)
path_out = os.path.abspath("data/toycon_media2.xml")
cobra.io.write_sbml_model(toycon, path_out)
