import cobra
import sys

#Read the model in
model_file_path = str(model_file_path)
toycon = cobra.io.read_sbml_model(model_file_path)
#Get objective and assert it is in the model
objective = str(objective)
try:
    toycon.reactions.get_by_id(objective)
except KeyError:
    print("The selected objective reaction is not in the model!")
#Change objective
toycon.objective = objective
#Resetting the model case
solution = toycon.optimize()
flux = solution.objective_value
fluxes = solution.fluxes.round(2).to_dict()
