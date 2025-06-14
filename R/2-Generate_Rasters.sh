#!/bin/bash --login
n="ais" # job name

# Note that 98 is the number of pieces the BT IDs were split up into. 
# Should probably not hardcode this...
for i in {2015..2024}
do
  sbatch --job-name=$n.$i --output=$n.$i.SLURMout --export=year=$i ./2-Generate_Rasters_datamachine.SB
done