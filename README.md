# Competition on Rugged Landscapes

## Summary
This is a simulation model from my thesis in management science in 2012. The paper describing the methods and in full results is included in the files and [available on academia.edu](https://www.academia.edu/2317035/Competition_on_Rugged_Landscapes_The_Dynamics_of_Product_Positioning).

The idea of the study is as follows (abstract of the paper):
> Many firms constantly compete through product positioning, but the resulting competitive interactions are not well understood, mainly because of the difficulties associated with modeling a fundamentally dynamic phenomenon. We present an agent-based model of firms' behavior when they compete through product positioning, i.e. horizontal differentiation. Our results suggest that the dynamics of competition through product positioning depend on the number of consumer niches in the market. In "rugged" markets with many niches, product designs stabilize as firms disperse to serve individual niches. There, competition has little effect on performance. By contrast, in ???smooth??? markets with few niches, product positioning remains volatile with firms jostling for favorable positions. In these markets competition is highly detrimental to performance and market leaders are frequently dethroned. By modeling the dynamics of product positioning we contribute to research on horizontal differentiation in industrial organization, as well as to work in evolutionary economics and population ecology.

## Model and Analysis

### Overview
This model has two parts: **simulation** and **analysis**. They can be called by running `crl_run.r`, and the model can be configured using `crl_config.r`.

*WARNING* - Running the script with default configurations (80 periods of competition, 1000 simulation runs) can take quite some time.

### Simulation
The simulation generates an agent-based model of companies competing for customers. 

Specifically, it generates "landscapes" of customers who have different preferences for a product in several dimensions, e.g. color, size etc. The "ruggedness" of the distribution of the customers in this space is varied across model settings (i.e. it is a tunable parameter in the model). A full description is in the paper.

Based on the customer "landscape" (varying numbers of) companies compete by positioning their (single) product in the preference dimensions (horizontal differentiation). Because customers select only one firm from which to purchase, companies' tactics influence each others profits, causing a dynamic competitive situation.

The simulation results (i.e. positions and payoffs of each firm in each period and for each simulation run) are stored in R dataframes, which are saved at the end of the simulation.

### Analysis
The analysis code uses the results from the simulation to perform analyses. Its output is two sets of plots that form the basis for the conclusions in the paper. 

### Other
The main model is based on the **NK class** of models (references in the paper), that is popular in management research. Several robustness checks mentioned in the paper can be replicated by changing the default values for the model parameters in `crl_config.r`.

The repository also contains a variation of the simulation that uses using a customer taste distribution in (quasi) continuous 2D space - a futher robustness check. Note that I have not yet checked the continuous variation since the last version of the paper, so it is unlikely to run as-is.

## Extensions
- Use a command-line argument to call simulation/analysis (and possibly define major parameters with defaults)
- Storing results in a database rather than dataframes would be substantially more efficient (in terms of storage, not sure about speed)
