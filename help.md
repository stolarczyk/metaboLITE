# **iNRG visualization app help**

Michal Stolarczyk <mjs5kd@virginia.edu>


## What is this?<a name="what"></a>

The *iNRG visualization app* is an R shiny app that aids the basic understanding of concepts conveyed in the following publication: *iNRG: A toy network capturing central energy metabolism for use with constraint-based methods.*

## Functionalities<a name="Functionalities"></a>

- **Network layout visualization**, which is intended to graphically present the interconnections between metabolites and reactions in the <a href="https://en.wikipedia.org/wiki/Metabolic_network_modelling#Genome-Scale_Metabolic_Reconstruction" target="_blank">genome-scale metabolic reconstruction</a>. Nodes of the graph represent metabolites and reactions whereas egdes - connections between them. 
- **Network stoichiometry visualization**, which is intended to depict the stoichiometric coefficients in view of the aforesaid network representation of the genome-scale metabolic reconstruction. The width of the edges correspond to the stoichiometic coefficients that determine ratios of each metabolite in the reactions.
- **Reaction knockouts (KOs) impact visualization**, which is intended to present the influence of the specific reaction KOs on genome-scale metabolic reconstruction architecture and fluxes in the model. By means of the <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf" target="_blank">Flux Balance Analysis</a> (FBA) this functionality can be used to detect either essential (when removed render the model carry 0 flux through the <a href="https://en.wikipedia.org/wiki/Flux_balance_analysis#Objective_function" target="_blank">objective function</a> or nonessential (when removed do not influence the effective flux value through the objective function) reactions. Additionaly, knockouts can simulate the complete enzyme inhibition that catalyzes the reaction being knocked out.
- **Media changes impact visualization**, which is intended to deptict the incluence of growth media changes on the model growth and fluxes through reactions. The media changes are performed by constraining the exchange reactions in the model during the FBA simulation. For example in order to check the influence of oxygen shortage on the model growth one needs to lower the upper (and lower) flux <a href=https://en.wikipedia.org/wiki/Flux_balance_analysis#Mathematical_description" target="_blank">bound(s)</a>  of the oxygen exchange reaction.
- **Gene expression influenece visualization**, which is intended to depict the impact of the pseudo-gene expression changes in the model, which directly influences the flux that is carried by the reaction catalyzed by the enzyme encoded by the gene in question. It is indended to give the user an idea of how the algorithms for gene expression integration influence the fluxes in in the model.



