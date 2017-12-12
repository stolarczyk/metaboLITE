import cobra
import sys

toycon = cobra.io.read_sbml_model("/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml")

reaction_ID = reaction_ID.upper()

if reaction_ID == "RESET":
  solution = toycon.optimize()
  flux = solution.objective_value
  fluxes = solution.fluxes.to_dict()
  path_removed = "/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml"
else:
  toycon.reactions.get_by_id(reaction_ID).knock_out()
  solution = toycon.optimize()
  flux = solution.objective_value
  fluxes = solution.fluxes.to_dict()
  path = "/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml"
  fragments = path.split("/")
  last_fragment = fragments[-1]
  fragments = fragments[:-1]
  filename = last_fragment.split(".")[0]
  filename = filename + "_removed_" + reaction_ID + ".xml"
  fragments[-1] = filename
  path_removed = '/'.join(fragments)
  toycon.reactions.get_by_id(reaction_ID).remove_from_model()
  cobra.io.write_sbml_model(toycon, path_removed)
