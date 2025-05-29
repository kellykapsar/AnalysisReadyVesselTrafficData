#!/bin/bash

# List of "type mode" pairs
arg_list=(
  "Cargo multi"
  "Fishing multi"
  "Other multi"
  "Tanker single"
  "Recreation single"
  "TugTow single"
  "Military single"
  "Unknown single"
)

# Loop through each pair and submit a job
for pair in "${arg_list[@]}"; do
  type=$(echo $pair | awk '{print $1}')
  mode=$(echo $pair | awk '{print $2}')
  sbatch 1-AISSubset_BeringStrait_VoyageLineCreation.SB "$type" "$mode"
done
