#!/bin/bash

# Run once, hold otherwise
if [ -f "already_ran" ]; then
    echo "Already ran the Entrypoint once. Holding indefinitely for debugging."
    cat
fi
sudo touch already_ran

sudo mkdir ./compare_results/
sudo ./_build/default/start.exe -k 100 -w 10 -output-dir ./compare_results/ -projects-dir ./projects/
