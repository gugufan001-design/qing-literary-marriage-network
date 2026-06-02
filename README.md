# Qing Literary Marriage Network

This repository provides the cleaned analytical dataset, output tables, figures, and R scripts used to reproduce the results reported in the manuscript:

**Cultural Sustainability Through Kinship Networks: A Quantitative Analysis of Marriage Strategies in Qing China's Literary Lineages (1644–1911)**

## Data source

The study is based on Xu Yanping's published reference work *Genealogical Networks of Literary Lineages in the Qing Dynasty* (2010). The original printed source records 5,289 marriage entries involving 766 literary-family entries.

The original printed source and full OCR text are not reproduced in this repository because they derive from a copyrighted published reference work. This repository provides the cleaned structured analytical dataset required to reproduce the reported network measures, regression models, robustness checks, and tables.

## Analytical dataset

After OCR correction, rule-based parsing, surname–location normalization, and removal of self-loops, the revised analytical dataset contains:

- 5,197 parseable marriage records
- 657 focal literary-family nodes
- 2,704 total family nodes including non-focal marriage partners
- 3,541 distinct weighted family-pair edges

## Repository structure

- `scripts/`: R scripts for parsing, network construction, regression analysis, robustness checks, diagnostics, and table generation.
- `data/`: Cleaned structured data used for analysis.
- `results/`: Output tables and robustness-check results used in the revised manuscript.
- `figures/`: Diagnostic and structural robustness figures.

## Main data files

The `data/` folder contains:

- `raw_entries_clean.csv`: Parsed entry-level marriage records.
- `marriage_edges_clean.csv`: Deduplicated weighted family-pair edge list.
- `family_attributes_clean.csv`: Node-level family attributes and network centrality measures.
- `network_summary.csv`: Global network statistics.
- `regression_dataset_focal_families.csv`: Dataset used for regression models among focal literary-family nodes.

## Main scripts

The `scripts/` folder contains:

- `qing_marriage_network_parser_v5.R`: Parses the OCR-based source corpus, normalizes family nodes, constructs the weighted marriage network, and outputs cleaned network data.
- `qing_marriage_regression_review_response.R`: Constructs the Literary Capital Proxy, runs regression models, robustness checks, diagnostics, discordant-case analysis, and structural robustness simulations.

## Reproduction steps

Run the scripts in the following order:

```r
source("scripts/qing_marriage_network_parser_v5.R")
source("scripts/qing_marriage_regression_review_response.R")
