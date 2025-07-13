# Meta-Analysis of Median Survival Times

This repository contains the data and code used for the analyses in "Meta-analysis of median survival times with inverse-variance weighting" by Sean McGrath, Jonathan Kimmelman, Omer Ozturk, Russell Steele, and Andrea Benedetti. 

---

## Environment Details

The analyses were run on R version 4.4.1 with the following packages: 

- **doParallel** (version 1.0.17)
- **doRNG** (version 1.8.6)
- **foreach** (version 1.5.2)
- **iterators** (version 1.0.14)
- **Matrix** (version 1.7-0)
- **metadat** (version 1.2-0)
- **metafor** (version 4.6-0)
- **numDeriv** (version 2016.8-1.1)
- **rngtools** (version 1.5.2)
- **survival** (version 3.6-4)
- **xtable** (version 1.8-4)

## Simulation Study

### Study-level simulations

#### Main Files

- ``run-sims-studylevel.R``: Runs the study-level simulations. Results are saved in ``sim-res-studylevel.RData``.
- ``run-sims-studylevel-bootstrap.R``: Runs the study-level simulations that evaluate the impact of the number of bootstrap replicates. Results are saved in ``sim-res-studylevel-bootstrap.RData``.
- ``analyze-results-studylevel.R``: Analyzes the simulation results.

#### Supporting Files

- ``settings-studylevel.R``: Sets the simulation settings (e.g., number of iterations, sample sizes).
- ``helper.R``: Contains helper functions used in the simulations.
- ``censoring-percentage.R``: Obtains the percentage of censoring.
- ``distributions.R``: Plots the event time distributions.

#### Computational Details

The study-level simulations took approximately 1.5 hours to run when parallelized across 20 CPU cores. 

### Meta-Analysis Simulations

#### Main Files

- ``run-sims-meta.R``: Runs the meta-analysis simulations. Results are saved in ``sim-res-meta.RData``.
- ``run-sims-meta-wrongtransform.R``: Runs the meta-analysis simulations where the form of the between-study heterogeneity is misspecified. Results are saved in ``sim-res-meta-wrongtransform.RData``.
- ``true-I2.R``: Obtains the true values of $I^2$. Results are saved in ``true-I2.RData``.
- ``analyze-results-meta.R`: Analyzes the simulation results.

#### Supporting Files

- ``settings-meta.R``: Sets the simulation settings (e.g., number of iterations, heterogeneity levels, etc.).
- ``helper.R``: Contains helper functions used in the simulations.
- ``true-effect-dist.R``: Obtains the distribution of the study-specific differences of medians and ratio of medians in the simulations where the form of the between-study heterogeneity is misspecified.

#### Computational Details

The meta-analysis simulations (each of ``run-sims-meta.R`` and ``run-sims-meta-wrongtransform.R``) took approximately 15 hours to run when parallelized across 20 CPU cores. The analyses for obtaining the true $I^2$ values took approximately 5 minutes to run when parallelized across 10 CPU cores. 

## Data Application

The data application uses the following files: 

- ``data-illustration.csv``: Contains the dataset.
- ``application.R``: Contains the code for the data application.

## Illustration in Appendix B

The illustration in Appendix B uses the file ``appendix-b.R``.
