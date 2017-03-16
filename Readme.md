# Competition on Rugged Landscapes

## Summary
This is a simulation model I built for my Ph.D. thesis in management science in 2012. The paper describing the methods and in full results is [available on academia.edu](https://www.academia.edu/2317035/Competition_on_Rugged_Landscapes_The_Dynamics_of_Product_Positioning).

The idea of the study is as follows (abstract of the paper):
> Many firms constantly compete through product positioning, but the resulting competitive interactions are not well understood, mainly because of the difficulties associated with modeling a fundamentally dynamic phenomenon. We present an agent-based model of firms' behavior when they compete through product positioning, i.e. horizontal differentiation. Our results suggest that the dynamics of competition through product positioning depend on the number of consumer niches in the market. In "rugged‟ markets with many niches, product designs stabilize as firms disperse to serve individual niches. There,
competition has little effect on performance. By contrast, in „smooth‟ markets with few niches, product positioning remains volatile with firms jostling for favorable positions. In these markets competition is highly detrimental to performance and market leaders are frequently dethroned. By modeling the dynamics of product positioning we contribute to research on horizontal differentiation in industrial organization, as well as to work in evolutionary economics and population ecology.

## Model and Analysis
### Overview
The code in this repository falls into two basic classes: **Simulation** and **analysis**.

The **simulation** code generates an agent-based model of companies competing for customers on aritificially generated landscapes of customers with varying distributions of taste (customer niches). The results are stored in R dataframes.

The **analysis** code uses the results from the simulation to generate analyses - notably plots - that form the basis for the conclusions in the paper.

The main model is based on the NK class of models (references in the paper), that is popular in management research. The repository also contains a variation of the simulation that uses using a customer taste distribution in (quasi) continuous 2D space - a robustness check.

### Detailed results
[watch this space]
