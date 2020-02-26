#!/bin/bash

# Run once, hold otherwise
if [ -f "already_ran" ]; then
    echo "Already ran the Entrypoint once. Holding indefinitely for debugging."
    cat
fi
sudo touch already_ran

cd ./projects/
for i in *.zip; do sudo unzip "$i" -d "${i%%.zip}"; done
cd ../

sudo mkdir ./compare_results/
sudo ./_build/default/start.exe -k $K -w $W -m $M -output-dir ./compare_results/ -projects-dir ./projects/ -file-type js -ignore-dir node_modules
