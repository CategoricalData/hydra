#!/usr/bin/env python3
"""Generate a categorized summary report of Hydra test results.

Usage:
    python src/test/python/test_summary_report.py

Or from any directory:
    cd hydra-python && python src/test/python/test_summary_report.py
"""

import subprocess
import re
from collections import defaultdict
from pathlib import Path

def generate_summary():
    """Run pytest and generate a categorized summary report."""
    # Ensure we're in the right directory
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent.parent.parent

    # Run pytest and capture output
    result = subprocess.run(
        ["pytest", "-v", "--tb=no", "--no-header"],
        capture_output=True,
        text=True,
        cwd=project_dir
    )

    # Parse test results
    categories = defaultdict(lambda: {"passed": 0, "failed": 0, "skipped": 0})

    for line in result.stdout.split('\n'):
        if '::test_' in line:
            # Extract test name and status
            match = re.search(r'::test_(.*?)\s+(PASSED|FAILED|SKIPPED)', line)
            if match:
                test_name, status = match.groups()

                # Determine category from test name
                if test_name.startswith('all_tests_formatting_tests'):
                    category = "Formatting Tests"
                elif test_name.startswith('all_tests_inference_tests_algebraic_terms'):
                    category = "Inference Tests - Algebraic Terms"
                elif test_name.startswith('all_tests_inference_tests_algorithm_w'):
                    category = "Inference Tests - Algorithm W"
                elif test_name.startswith('all_tests_inference_tests_expected_failures'):
                    category = "Inference Tests - Expected Failures"
                elif test_name.startswith('all_tests_inference_tests_fundamentals'):
                    category = "Inference Tests - Fundamentals"
                elif test_name.startswith('all_tests_inference_tests_kernel_examples'):
                    category = "Inference Tests - Kernel Examples"
                elif test_name.startswith('all_tests_inference_tests_nominal_types'):
                    category = "Inference Tests - Nominal Types"
                elif test_name.startswith('all_tests_inference_tests_simple_terms'):
                    category = "Inference Tests - Simple Terms"
                elif test_name.startswith('all_tests_lib_tests'):
                    category = "Library Tests"
                else:
                    category = "Python-Specific Tests"

                status_lower = status.lower()
                categories[category][status_lower] += 1

    # Print summary
    print("\n" + "="*80)
    print("TEST SUMMARY BY CATEGORY")
    print("="*80 + "\n")

    # Sort categories
    sorted_categories = sorted(categories.items())

    total_passed = 0
    total_failed = 0
    total_skipped = 0

    for category, counts in sorted_categories:
        total = counts['passed'] + counts['failed'] + counts['skipped']
        total_passed += counts['passed']
        total_failed += counts['failed']
        total_skipped += counts['skipped']

        print(f"{category}")
        print(f"  {'Passed:':<12} {counts['passed']:>3}/{total:<3} "
              f"({100*counts['passed']//total if total > 0 else 0:>3}%)")
        print(f"  {'Failed:':<12} {counts['failed']:>3}/{total:<3} "
              f"({100*counts['failed']//total if total > 0 else 0:>3}%)")
        print(f"  {'Skipped:':<12} {counts['skipped']:>3}/{total:<3} "
              f"({100*counts['skipped']//total if total > 0 else 0:>3}%)")
        print()

    # Print totals
    grand_total = total_passed + total_failed + total_skipped
    print("="*80)
    print("TOTAL:")
    print(f"  {'Passed:':<12} {total_passed:>3}/{grand_total:<3} "
          f"({100*total_passed//grand_total if grand_total > 0 else 0:>3}%)")
    print(f"  {'Failed:':<12} {total_failed:>3}/{grand_total:<3} "
          f"({100*total_failed//grand_total if grand_total > 0 else 0:>3}%)")
    print(f"  {'Skipped:':<12} {total_skipped:>3}/{grand_total:<3} "
          f"({100*total_skipped//grand_total if grand_total > 0 else 0:>3}%)")
    print("="*80 + "\n")


if __name__ == "__main__":
    generate_summary()
