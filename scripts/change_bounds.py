import cobra
import sys

#Read the model in
toycon = cobra.io.read_sbml_model(str(model_file_path))
#Set the new bounds
lower = int(lb)
upper = int(ub)
toycon.reactions.get_by_id(reaction_ID).bounds = (lower, upper)
#Get objective and assert it is in the model
objective = str(objective)
try:
    toycon.reactions.get_by_id(objective)
except KeyError:
    print("The selected objective reaction is not in the model!")
#Change objective
toycon.objective = objective
#Run the FBA
flux = toycon.optimize().f
#Save the results
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()
