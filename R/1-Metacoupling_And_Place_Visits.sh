#!/bin/bash

# List of "type mode" pairs
arg_list=(
  "Cargo"
  "Fishing"
  "Other"
  "Tanker"
  "Recreation"
  "TugTow"
  "Military"
  "Unknown"
)

# Loop through each pair and submit a job
for pair in "${arg_list[@]}"; do
  type=$(echo $pair | awk '{print $1}')
  sbatch 1-AISSubset_BeringStrait_VoyageLineCreation.SB "$type"
done
