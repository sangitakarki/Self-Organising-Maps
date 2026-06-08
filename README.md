# Unsupervised Machine Learning for Clustering Eukaryotic Protein Families Using Self-Organizing Maps
## Overview
This repository contains an R-based unsupervised machine learning workflow for clustering eukaryotic protein families (Pfam domains) based on their distribution across cellular lineages and nucleocytoviruses using Self-Organizing Maps (SOMs).The objective of this analysis was to identify groups of protein families exhibiting similar distribution patterns without predefined class labels. By applying SOM-based clustering and hierarchical clustering, the workflow uncovers hidden structure within high-dimensional protein distribution data and reveals distinct groups of shared protein families.

## Biological Background
To identify protein families shared between eukaryotes and nucleocytoviruses, Pfam domains were surveyed across a large collection of eukaryotic genomes and viral genomes.
A total of
763 Pfam domains were detected in more than 95% of surveyed eukaryotic genomes.
500 of these Pfam domains were also identified in at least one nucleocytovirus genome.
The distribution of these shared protein families was used as input for unsupervised clustering analysis.

## Workflow
#### Pfam Distribution Matrix
   
A matrix was constructed containing the occurrence patterns of shared Pfam domains across taxonomic groups.

#### Self-Organizing Map (SOM)

A Self-Organizing Map was trained using the kohonen package in R to project high-dimensional distribution data onto a two-dimensional grid while preserving similarity relationships among Pfam domains.

#### Cluster Identification

SOM codebook vectors were extracted and clustered using hierarchical clustering.
The optimal number of clusters was determined using the elbow method.

#### Visualization

Cluster assignments were visualized using:
SOM cluster maps and Hierarchically clustered heatmaps

## Software Requirements
R (≥ 3.6)
kohonen
pheatmap

Install dependencies:

install.packages("kohonen")

install.packages("pheatmap")
## Input
Input data provided as a matrix example:
	
<img width="481" height="97" alt="image" src="https://github.com/user-attachments/assets/a3f704ca-bd3e-4c45-ae31-b98cf94d1b7f" />

## Output
The workflow generates:

SOM cluster visualization (SOM.pdf)
<img width="705" height="779" alt="image" src="https://github.com/user-attachments/assets/b19fee29-06f2-4373-b8b3-f01a22cdb80d" />

Heatmap of clustered Pfam domains (clusters.pdf)

Assigned cluster table (clusters.txt)
## Applications
This workflow can be adapted for:

Comparative genomics

Protein family distribution analyses

Functional genomics

Viral evolution studies

Exploratory clustering of high-dimensional biological datasets
## Citation

If you use this workflow, please cite the associated publication

Karki, Sangita, and Frank O. Aylward. "Evolution of ubiquitin, cytoskeleton, and vesicular trafficking machinery in giant viruses." Journal of Virology 99, no. 3 (2025): e01715-24.

https://doi.org/10.1128/jvi.01715-24
