# A Mixing Time Lower Bound for a Simplified Version of BART

This repository contains the code for all the simulation in the paper [A Mixing Time Lower Bound for a Simplified Version of BART](https://arxiv.org/abs/2210.09352).
We provide a command line interface to easily produce the figures in this paper:
1. RMSE - plots comparing the kernel density estimates across chains can be generated using:
```bash
Rscript code/bart_sim.R -p rmse -d "echo_months"
```  
2. Cusum - cusum diagnostic plots [(Yu & Mykland)](https://link.springer.com/article/10.1023/A:1008917713940) can be generated using:
```bash
Rscript code/bart_sim.R -p cusum -d "echo_months"
```  
3. First Split Index - plots showing the index of variable on which the first split is made can be generated using:
```bash
Rscript code/bart_sim.R -p first_split -d "echo_months"
```  
4. Gelman-Rubin (**takes very long**) - a plots showing the Gelman-Rubin diagnostic across the datasets we study can be generated using:
```bash
Rscript code/bart_sim.R -p gr
```

For all the commands above, BART hyperprameters can be passed throught the command line interface, see:
```bash
Rscript code/bart_sim.R -h
```
for more details.
