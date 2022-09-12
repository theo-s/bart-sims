#!/bin/bash
#for databaseName in "satellite_image" "echo_months""california_housing" "breast_tumor"; do
for databaseName in "california_housing"; do
  for n in 200 2000; do
	  for p in 1; do
#    for n in 1000000000; do
    for n_tree in 1; do
      for run in {6..20}; do
  sbatch bart_sim.sh $databaseName $n $p $n_tree $run
 # sbatch bart_sim_synthetic.sh $databaseName $n $n_tree
 done
  done
  done
done
done
