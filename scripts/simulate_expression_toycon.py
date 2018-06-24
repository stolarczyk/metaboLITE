import cobra
import sys

#Read the model in
model = cobra.io.read_sbml_model(str(model_file_path))
# Find the reactions that are associated with the selected gene
reactions_associated = list(model.genes.get_by_id(str(gene_ID)).reactions)
#Get objective and assert it is in the model
objective = str(objective)
try:
    model.reactions.get_by_id(objective)
except KeyError:
    print("The selected objective reaction is not in the model!")
#Change objective
model.objective = objective
for rxn in reactions_associated:
    # Get the ID
    ID_ori = rxn.id
    # Get the original flux bounds
    bounds_ori = rxn.bounds
    # Get the gene-protein-reaction rule
    gpr_ori = rxn.gene_reaction_rule
    # All the reactions are associated with just one gene, so expression changes of each gene affect the fluxes
    lb = bounds_ori[0] * bound
    ub = bounds_ori[1] * bound
    model.reactions.get_by_id(ID_ori).bounds = (lb, ub)

# Perform FBA
flux = model.optimize().f
# Save the results
fluxes = model.optimize().fluxes.round(2)
fluxes = fluxes.to_dict()

