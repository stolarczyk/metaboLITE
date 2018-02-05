import cobra
import sys

#Get media
media_type = str(media_type)
#Get model path
model_file_path = str(model_file_path)
model_file_path_cut = model_file_path[:-4]
#Read the proper model in
toycon = cobra.io.read_sbml_model(model_file_path_cut + "_" + media_type + ".xml")
#Run FBA
flux = toycon.optimize().objective_value
#Save the results
fluxes = toycon.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()

