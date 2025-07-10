#!/usr/bin/env python3
"""
Script to generate Hydra tabular-to-graph mapping prompts.

Usage:
    python3 generate_prompt.py <data_directory>
    
Example:
    python3 generate_prompt.py data/genpg/health/
"""

import os
import sys
import subprocess
import tempfile
from pathlib import Path

def run_demo_script(data_dir):
    """Run the summarize_tables.py script to generate table summaries."""
    demo_script = "src/main/python/hydra/demos/genpg/summarize_tables.py"
    
    if not os.path.exists(demo_script):
        raise FileNotFoundError(f"Demo script not found at: {demo_script}")
    
    try:
        result = subprocess.run([
            "python3", demo_script, data_dir
        ], capture_output=True, text=True, check=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Error running demo script: {e.stderr}")

def read_haskell_file(filename):
    """Read a Haskell file from the examples directory."""
    base_path = "src/main/haskell/Hydra.Ext.Demos/GenPG"
    file_path = os.path.join(base_path, filename)
    
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Haskell file not found: {file_path}")
    
    with open(file_path, 'r') as f:
        return f.read()

def get_sales_example_data():
    """Get the sales domain example data by running demo on sales directory."""
    sales_data_dir = "data/genpg/sources/sales"
    return run_demo_script(sales_data_dir)

def generate_prompt(new_data_dir):
    """Generate the complete prompt with sales examples and new domain data."""
    
    # Get sales example data
    try:
        sales_data = get_sales_example_data()
    except Exception as e:
        print(f"Warning: Could not generate sales example data: {e}")
        print("Using placeholder for sales data...")
        sales_data = "[Sales data would be generated here]"
    
    # Get new domain data
    new_domain_data = run_demo_script(new_data_dir)
    
    # Read Haskell example files
    try:
        database_schema = read_haskell_file("ExampleDatabaseSchema.hs")
        graph_schema = read_haskell_file("ExampleGraphSchema.hs")
        mapping = read_haskell_file("ExampleMapping.hs")
    except FileNotFoundError as e:
        print(f"Error reading Haskell files: {e}")
        sys.exit(1)
    
    # Generate the prompt
    prompt = f"""I would like you to help me map my tabular data into a strongly-typed property graph using Hydra.
I will first help you understand the framework by giving you the following:
(1) Some sample CSV data from a Sales domain
(2) A lightweight database schema for the Sales data, called generatedTableSchemas,
(3) A well-designed property graph schema for the Sales data, called generatedGraphSchema, and finally
(4) A well-designed mapping for the Sales data, called generatedGraphMapping.
After that, I will give you (5), a sample of the actual CSV data I need to map into the graph.
Please give me three files: DatabaseSchema.hs, GraphSchema.hs and Mapping.hs, all in the package
Hydra.Ext.Demos.GenPG.Generated, in the directory src/gen-main/haskell/Hydra.Ext.Demos/GenPG/Generated.
Feel free to ask any clarifying questions as needed before you generate the files.

################################################################################
1. SAMPLE OF SALES DATA

{sales_data}

################################################################################
2. SALES DATABASE SCHEMA

{database_schema}

################################################################################
3. SALES GRAPH SCHEMA

{graph_schema}

################################################################################
4. SALES MAPPING

{mapping}

################################################################################
SAMPLE OF NEW DOMAIN DATA

{new_domain_data}"""

    return prompt

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 generate_prompt.py <data_directory>")
        print("Example: python3 generate_prompt.py data/genpg/health/")
        sys.exit(1)
    
    data_dir = sys.argv[1]
    
    if not os.path.exists(data_dir):
        print(f"Error: Data directory not found: {data_dir}")
        sys.exit(1)
    
    try:
        prompt = generate_prompt(data_dir)
        print(prompt)
    except Exception as e:
        print(f"Error generating prompt: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()

