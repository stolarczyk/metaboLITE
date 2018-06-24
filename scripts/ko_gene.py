import cobra
import sys

# Read the model in
model_file_path = str(model_file_path)
toycon = cobra.io.read_sbml_model(model_file_path)
objective = str(objective)
try:
    toycon.reactions.get_by_id(objective)
except KeyError:
    print("The selected objective reaction is not in the model!")
#Change objective
toycon.objective = objective
gene_ID = str(reaction_ID)
# Resetting the model case
if gene_ID == "RESET":
    solution = toycon.optimize()
    flux = solution.objective_value
    fluxes = solution.fluxes.to_dict()
# Otherwise
else:
    toycon.genes.get_by_id(reaction_ID).knock_out()
    solution = toycon.optimize()
    flux = solution.objective_value
    fluxes = toycon.optimize().fluxes.round(2)
    fluxes = fluxes.to_dict()
