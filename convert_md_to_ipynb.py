import json
import sys
import re

def convert(filename):
    with open(filename, "r") as f:
        lines = f.readlines()

    cells = []
    
    badges_md = "[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/pcarbonn/pyDatalog/blob/master/" + filename.replace('.md', '.ipynb') + ") [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/pcarbonn/pyDatalog/master?filepath=" + filename.replace('.md', '.ipynb') + ")"
    cells.append({"cell_type": "markdown", "metadata": {}, "source": [badges_md]})
    
    setup_code = "%pip install --upgrade pip\n%pip install --upgrade pyDatalog"
    cells.append({"cell_type": "code", "execution_count": None, "metadata": {}, "outputs": [], "source": [setup_code]})

    in_code_block = False
    current_md = []
    current_code = []
    
    for line in lines:
        if line.startswith("```python"):
            in_code_block = True
            if current_md:
                cells.append({"cell_type": "markdown", "metadata": {}, "source": current_md.copy()})
                current_md = []
            continue
        elif in_code_block and line.startswith("```"):
            in_code_block = False
            if current_code:
                cells.append({"cell_type": "code", "execution_count": None, "metadata": {}, "outputs": [], "source": current_code.copy()})
                current_code = []
            continue
            
        if in_code_block:
            if line.startswith(">>> "):
                current_code.append(line[4:])
            elif line.startswith("... "):
                current_code.append(line[4:])
            elif line.startswith("..."):
                current_code.append(line[3:])
        else:
            current_md.append(line)

    if current_md:
        cells.append({"cell_type": "markdown", "metadata": {}, "source": current_md})
        
    nb = {
        "cells": cells,
        "metadata": {
            "kernelspec": {
                "display_name": "Python 3",
                "language": "python",
                "name": "python3"
            }
        },
        "nbformat": 4,
        "nbformat_minor": 5
    }
    
    outname = filename.replace(".md", ".ipynb")
    with open(outname, "w") as f:
        json.dump(nb, f, indent=1)

for f in sys.argv[1:]:
    convert(f)
