import cobra
import sys

model_file_path = str(model_file_path)
toycon = cobra.io.read_sbml_model(model_file_path)

reaction_ID = reaction_ID.upper()

if reaction_ID == "RESET":
  solution = toycon.optimize()
  flux = solution.objective_value
  fluxes = solution.fluxes.to_dict()
  path_removed = model_file_path
else:
  if sys.platform.startswith('win'):
    toycon.reactions.get_by_id(reaction_ID).knock_out()
    solution = toycon.optimize()
    flux = solution.objective_value
    fluxes = toycon.optimize().fluxes.round(2)
    fluxes = fluxes.to_dict()
    path = model_file_path
    fragments = path.split("\\")
    last_fragment = fragments[-1]
    fragments = fragments[:-1]
    filename = last_fragment.split(".")[0]
    filename = filename + "_removed_" + reaction_ID + ".xml"
    fragments.append(filename)
    path_removed = "\\".join(fragments)
    toycon.reactions.get_by_id(reaction_ID).remove_from_model()
    cobra.io.write_sbml_model(toycon, path_removed)
  else:
    toycon.reactions.get_by_id(reaction_ID).knock_out()
    solution = toycon.optimize()
    flux = solution.objective_value
    #Save the results
    fluxes = toycon.optimize().fluxes.round(2)
    fluxes = fluxes.to_dict()
    path = model_file_path
    fragments = path.split("/")
    last_fragment = fragments[-1]
    fragments = fragments[:-1]
    filename = last_fragment.split(".")[0]
    filename = filename + "_removed_" + reaction_ID + ".xml"
    fragments.append(filename)
    path_removed = "/".join(fragments)
    toycon.reactions.get_by_id(reaction_ID).remove_from_model()
    cobra.io.write_sbml_model(toycon, path_removed)
    
