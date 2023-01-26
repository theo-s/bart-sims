#!/bin/bash
#SBATCH --job-name=bart_sim
#SBATCH --mail-type=ALL
#SBATCH --mail-user=omer_ronen@berkeley.edu
#SBATCH -o bart_sim.out #File to which standard out will be written
#SBATCH -e bart_sim.err #File to which standard err will be written
#SBATCH -p high
##SBATCH --mem-per-cpu=100g
##SBATCH -C
##SBATCH --gres=gpu:A100:1
Rscript /accounts/campus/omer_ronen/projects/bart/bart-sims/code/bart_sim.R --dataset $1 -n $2 -p $3 --n_tree $4 --plot_type coverage --run $5
