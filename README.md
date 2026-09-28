# Normative CBF Trajectory

Code accompanying the study **“Normative Cerebral Perfusion Across the Lifespan”**.

This repository contains the analysis code used to construct lifespan normative models of cerebral blood flow (CBF) from arterial spin labeling (ASL) MRI data, characterize age-related CBF trajectories, and quantify individual deviations from normative perfusion patterns.

## Repository structure

- `main_code/`  
  Main scripts for data processing, normative modeling, statistical analyses, and generation of the primary study results.

- `source_code/`  
  Supporting functions and auxiliary scripts required by the main analysis pipeline.

## Overview

The analysis framework models age-related variation in cerebral perfusion across the human lifespan using generalized additive models for location, scale, and shape (GAMLSS). The models account for demographic and acquisition-related covariates and provide individualized normative deviation scores.

The code includes analyses for:

- Lifespan trajectories of global and regional CBF
- Estimation of normative CBF distributions
- Individualized CBF deviation (z-score) calculation
- Evaluation of acquisition and site effects
- Application of normative models to clinical populations
- Generation of figures and statistical results reported in the manuscript

## Requirements

The analyses were primarily implemented in R, with additional scripts where indicated in the corresponding folders.

Required packages and software dependencies are specified within the individual analysis scripts.

## Usage

Scripts should generally be run from the corresponding analysis directories. File paths and dataset locations may need to be modified according to the local computing environment.

Because several datasets used in this study are subject to data-use agreements, the raw imaging and participant-level data are not distributed with this repository.

## Data availability

The study integrates ASL MRI data from multiple publicly available and institutionally governed datasets. Access to the original datasets should be obtained from their respective data repositories or data custodians.

Source data underlying the published figures are provided separately with the article where applicable.

## Citation

If you use this code, please cite:

> Zeng X, et al. Zeng X, Li Y, Hua L, Lu R, Franco LL, Kochunov P, Chen S, Detre JA, Wang Z. Normative Cerebral Perfusion Across the Lifespan. ArXiv [Preprint]. 2025 Feb 12:arXiv:2502.08070v1. PMID: 39990798; PMCID: PMC11844630.

## License

Please see the `LICENSE` file for terms of use.
