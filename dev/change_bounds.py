import cobra
import sys

toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml")
lower = int(lb)
upper = int(ub)
toycon.reactions.get_by_id(reaction_ID).bounds = (lower, upper)

flux = toycon.optimize().f
