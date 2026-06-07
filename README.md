# Self-Organising-Maps
# Overview
This repository contains an R-based unsupervised machine learning workflow for clustering eukaryotic protein families (Pfam domains) based on their distribution across cellular lineages and nucleocytoviruses using Self-Organizing Maps (SOMs).The objective of this analysis was to identify groups of protein families exhibiting similar distribution patterns without predefined class labels. By applying SOM-based clustering and hierarchical clustering, the workflow uncovers hidden structure within high-dimensional protein distribution data and reveals distinct groups of shared protein families.

# Biological Background
To identify protein families shared between eukaryotes and nucleocytoviruses, Pfam domains were surveyed across a large collection of eukaryotic genomes and viral genomes.
A total of
763 Pfam domains were detected in more than 95% of surveyed eukaryotic genomes.
500 of these Pfam domains were also identified in at least one nucleocytovirus genome.
The distribution of these shared protein families was used as input for unsupervised clustering analysis.

# Workflow
# Generate Pfam Distribution Matrix
   
A matrix was constructed containing the occurrence patterns of shared Pfam domains across taxonomic groups.

Self-Organizing Map (SOM)

A Self-Organizing Map was trained using the kohonen package in R to project high-dimensional distribution data onto a two-dimensional grid while preserving similarity relationships among Pfam domains.

Cluster Identification

SOM codebook vectors were extracted and clustered using hierarchical clustering.
The optimal number of clusters was determined using the elbow method.

Visualization

Cluster assignments were visualized using:
SOM cluster maps
Hierarchically clustered heatmaps
