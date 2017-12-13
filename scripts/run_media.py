import cobra
import sys

media_type = str(media_type)
model_file_path = str(model_file_path)
model_file_path_cut = model_file_path[:-4]
toycon = cobra.io.read_sbml_model(model_file_path_cut + "_" + media_type + ".xml")
flux = toycon.optimize().objective_value
fluxes = toycon.optimize().fluxes.to_dict()

