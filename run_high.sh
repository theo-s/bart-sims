#!/bin/bash
#SBATCH --job-name=bart_high
#SBATCH --mail-type=ALL
#SBATCH --mail-user=theo_s@berkeley.edu
#SBATCH -o high.out #File to which standard out will be written
#SBATCH -e high.err #File to which standard err will be written
#SBATCH -p high
##SBATCH --mem-per-cpu=100g
##SBATCH -C
##SBATCH --gres=gpu:A100:1
Rscript code/bart_coverage.R -d "atheyhigh" -r 8
