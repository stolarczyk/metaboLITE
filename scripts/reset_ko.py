import cobra
import sys

#Read the model in
model_file_path = str(model_file_path)
toycon = cobra.io.read_sbml_model(model_file_path)

#Resetting the model case
solution = toycon.optimize()
flux = solution.objective_value
fluxes = solution.fluxes.to_dict()
