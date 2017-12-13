import cobra
import sys

toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml")
lower = int(-100)
upper = int(100)
toycon.reactions.get_by_id("E2").bounds = (lower, upper)
lower = int(-100)
upper = int(100)
toycon.reactions.get_by_id("E4").bounds = (lower, upper)
cobra.io.write_sbml_model(toycon,"/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon_media2.xml")
