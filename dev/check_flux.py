import cobra
import sys

toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/data/toycon.xml")
flux = toycon.optimize().f

