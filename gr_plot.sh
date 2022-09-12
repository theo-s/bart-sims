#!/bin/bash
#SBATCH --job-name=gr_plot
#SBATCH --mail-type=ALL
#SBATCH --mail-user=omer_ronen@berkeley.edu
#SBATCH -o gr.out #File to which standard out will be written
#SBATCH -e gr.err #File to which standard err will be written

Rscript /accounts/campus/omer_ronen/projects/bart/bart-sims/code/bart_sim.R --n_tree 200 --plot_type gr