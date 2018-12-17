import cobra
import sys

#Read the model in
toycon = cobra.io.read_sbml_model(str(model_file_path))
#Run the FBA
flux = toycon.optimize().objective_value

