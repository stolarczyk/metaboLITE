# **metaboLITE web app**

Michal Stolarczyk <mjs5kd@virginia.edu>

![What is this](docs/img/logonav.png)

## What is this?

*metaboLITE* is an R shiny application that aids the basic understanding of concepts under genome-scale metabolic modeling

## Functionalities

- **Network layout visualization**, which is intended to graphically present the interconnections between metabolites and reactions in the [genome-scale metabolic reconstruction](https://en.wikipedia.org/wiki/Metabolic_network_modelling#Genome-Scale_Metabolic_Reconstruction). Nodes of the graph represent metabolites and reactions whereas egdes - connections between them. 
- **Network stoichiometry visualization**, which is intended to depict the stoichiometric coefficients in view of the aforesaid network representation of the genome-scale metabolic reconstruction. The width of the edges correspond to the stoichiometic coefficients that determine ratios of each metabolite in the reactions.
- **Reaction knockouts (KOs) impact visualization**, which is intended to present the influence of the specific reaction KOs on genome-scale metabolic reconstruction architecture and fluxes in the model. By means of the [Flux Balance Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf) (FBA) this functionality can be used to detect either essential (when removed render the model carry 0 flux through the [objective function](https://en.wikipedia.org/wiki/Flux_balance_analysis#Objective_function)) or nonessential (when removed do not influence the effective flux value through the objective function) reactions. Additionaly, knockouts can simulate the complete enzyme inhibition that catalyzes the reaction being knocked out.
- **Media changes impact visualization**, which is intended to deptict the incluence of growth media changes on the model growth and fluxes through reactions. The media changes are performed by constraining the exchange reactions in the model during the FBA simulation. For example in order to check the influence of oxygen shortage on the model growth one needs to lower the upper (and lower) flux [bound(s)](https://en.wikipedia.org/wiki/Flux_balance_analysis#Mathematical_description) of the oxygen exchange reaction.
- **Gene expression influence visualization**, which is intended to depict the impact of the pseudo-gene expression changes in the model, which directly influences the flux that is carried by the reaction catalyzed by the enzyme encoded by the gene in question. It is indended to give the user an idea of how the algorithms for gene expression integration influence the fluxes in in the model.

## Installation

Detailed installation structions can be found on the [project website](https://michalstolarczyk.github.io/metaboLITE/)

