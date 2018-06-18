import cobra
import sys

#Read the model in
toycon = cobra.io.read_sbml_model(str(model_file_path))
#Get original bounds
bounds = toycon.reactions.get_by_id(reaction_ID).bounds
#Calculate new bounds (influenced by the pseudo-expression)
lb = bounds[0]*bound
ub = bounds[1]*bound
#Apply them
toycon.reactions.get_by_id(reaction_ID).bounds = (lb, ub)
#Perform FBA
flux = toycon.optimize().f
#Save the results
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()
