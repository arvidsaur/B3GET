#!/bin/bash -l        
#PBS -l walltime=0:30:00,nodes=1:ppn=1,mem=10gb 
#PBS -m abe 
#PBS -M crou0048@umn.edu 
#PBS -N test_1
#PBS -q mesabi
Desktop/NetLogo\ 6.1.1-64/NetLogo\ 6.1.1/netlogo-headless.sh --model Desktop/B3GET/code/B3GET.nlogo --experiment WORLD-TEST
