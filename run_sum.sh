#!/bin/bash
#SBATCH --job-name=bart_sum
#SBATCH --mail-type=ALL
#SBATCH --mail-user=theo_s@berkeley.edu
#SBATCH -o sum.out #File to which standard out will be written
#SBATCH -e sum.err #File to which standard err will be written
#SBATCH -p high
##SBATCH --mem-per-cpu=100g
##SBATCH -C
##SBATCH --gres=gpu:A100:1
Rscript code/bart_coverage.R -d "sum" -r 8
