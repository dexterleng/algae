#!/bin/bash

sudo mkdir ./compare_results/
sudo ./_build/default/start.exe -k 100 -w 10 -output-dir ./compare_results/ -projects-dir ./projects/
