import cobra
import sys

#Read the model in
model_file_path = str(model_file_path)
toycon = cobra.io.read_sbml_model(model_file_path)

reaction_ID = reaction_ID.upper()
objective = str(objective)
try:
    toycon.reactions.get_by_id(objective)
except KeyError:
    print("The selected objective reaction is not in the model!")
#Change objective
toycon.objective = objective

#Resetting the model case
if reaction_ID == "RESET":
  solution = toycon.optimize()
  flux = solution.objective_value
  fluxes = solution.fluxes.to_dict()
  path_removed = model_file_path
#Otherwise
else:
#OS check
  if sys.platform.startswith('win'):
#KO the reaction
    toycon.reactions.get_by_id(reaction_ID).knock_out()
#Run FBA
    solution = toycon.optimize()
#Save the results
    flux = solution.objective_value
    fluxes = toycon.optimize().fluxes.round(2)
    fluxes = fluxes.to_dict()
#Saving the KO model in appropriate path
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
    
