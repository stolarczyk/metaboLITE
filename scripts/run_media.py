import cobra
import sys

media_type = str(media_type)
model_file_path = str(model_file_path)
model_file_path_cut = model_file_path[:-4]
toycon = cobra.io.read_sbml_model(model_file_path_cut + "_" + media_type + ".xml")
flux = toycon.optimize().objective_value
#Save the results
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()

