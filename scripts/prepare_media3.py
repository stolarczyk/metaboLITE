import cobra
import sys

toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml")
lower = int(0)
upper = int(100)
toycon.reactions.get_by_id("E1").bounds = (lower, upper)
lower = int(-100)
upper = int(0)
toycon.reactions.get_by_id("E5").bounds = (lower, upper)
cobra.io.write_sbml_model(toycon,"/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon_media3.xml")
