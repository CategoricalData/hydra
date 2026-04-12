import os
import csv
import sys

def process_csv_files(directory_path):
    # Check if the directory exists
    if not os.path.isdir(directory_path):
        print(f"Error: The directory '{directory_path}' does not exist.")
        return

    # Find all CSV files in the directory (not in subdirectories)
    csv_files = [file for file in os.listdir(directory_path) if file.endswith('.csv')]

    if not csv_files:
        print(f"No CSV files found in '{directory_path}'.")
        return

    print(f"Found {len(csv_files)} CSV files:")

    # Process each CSV file
    for file_name in csv_files:
        file_path = os.path.join(directory_path, file_name)
        print(f"\n{'='*50}")
        print(f"File: {file_name}")
        print(f"{'='*50}")

        try:
            with open(file_path, 'r', newline='', encoding='utf-8') as csvfile:
                csv_reader = csv.reader(csvfile)

                # Print the first 10 lines
                for i, row in enumerate(csv_reader):
                    if i < 10:
                        print(f"Line {i+1}: {row}")
                    else:
                        break

                # Add a note if there are more lines
                csvfile.seek(0)
                total_lines = sum(1 for _ in csv_reader)
                if total_lines > 5:
                    print(f"... and {total_lines - 5} more lines.")

        except Exception as e:
            print(f"Error reading file '{file_name}': {e}")

if __name__ == "__main__":
    # Get directory path from command line argument or use current directory
    if len(sys.argv) > 1:
        directory_path = sys.argv[1]
    else:
        directory_path = "../../../../haskell/Hydra/Ext/Demos/GenPG"

    process_csv_files(directory_path)