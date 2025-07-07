#!/bin/bash
n="ais"

# Create a list of year-month combos
> job_list.txt
for year in {2015..2024}; do
  for month in {01..12}; do
    echo "$year $month" >> job_list.txt
  done
done

# Count total number of tasks
total_jobs=$(wc -l < job_list.txt)

# Submit a single array job
sbatch --array=0-$(($total_jobs - 1)) \
       --job-name=$n.array \
       --output=$n.%A_%a.SLURMout \
       ./2-Generate_Rasters_datamachine.SB
