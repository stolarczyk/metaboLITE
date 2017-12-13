import cobra
import sys

media_type = str(media_type)
toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon_" + media_type + ".xml")
flux = toycon.optimize().objective_value

