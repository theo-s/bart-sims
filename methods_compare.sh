#!/bin/bash
#SBATCH --job-name=bart_methods
#SBATCH --mail-type=ALL
#SBATCH --mail-user=omer_ronen@berkeley.edu
#SBATCH -o bart_sim.out #File to which standard out will be written
#SBATCH -e bart_sim.err #File to which standard err will be written
#SBATCH -p jsteinhardt
#SBATCH -C mem768g
Rscript /accounts/campus/omer_ronen/projects/bart/bart-sims/code/methods_compare.R
