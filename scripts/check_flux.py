import cobra
import sys

toycon = cobra.io.read_sbml_model(str(model_file_path))
flux = toycon.optimize().f

