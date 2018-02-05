import cobra
import sys

#Read the model in
toycon = cobra.io.read_sbml_model(str(model_file_path))
#Set the new bounds
lower = int(lb)
upper = int(ub)
toycon.reactions.get_by_id(reaction_ID).bounds = (lower, upper)
#Run the FBA
flux = toycon.optimize().f
#Save the results
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()
