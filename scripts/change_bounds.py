import cobra
import sys

toycon = cobra.io.read_sbml_model(str(model_file_path))
lower = int(lb)
upper = int(ub)
toycon.reactions.get_by_id(reaction_ID).bounds = (lower, upper)

flux = toycon.optimize().f
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()
