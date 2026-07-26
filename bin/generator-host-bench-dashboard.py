#!/usr/bin/env python3
"""Dashboard for bin/bench-generator-hosts.sh results.json.

Usage: generator-host-bench-dashboard.py <results.json>
"""
import json
import sys


def fmt_ms(ms):
    if ms < 1000:
        return f"{ms}ms"
    return f"{ms / 1000.0:.1f}s"


def main():
    if len(sys.argv) != 2:
        print("Usage: generator-host-bench-dashboard.py <results.json>", file=sys.stderr)
        sys.exit(1)
    with open(sys.argv[1]) as f:
        data = json.load(f)

    target = data["target"]
    hosts = data["hosts"]
    host_names = list(hosts.keys())
    all_pkgs = sorted({pkg for h in hosts.values() for pkg in h["packages"]})

    print("=" * 70)
    print(f"  #459 generator-host benchmark: target={target}")
    print("=" * 70)
    print()
    print("  Setup time (one-time: stack build / gradle resolve+compile):")
    for h in host_names:
        print(f"    {h:10s} {fmt_ms(hosts[h]['setupMs'])}")
    print()
    print("  Transform time (median over runs, per package):")
    header = "    " + f"{'Package':16s}" + "".join(f"{h:>14s}" for h in host_names)
    print(header)
    for pkg in all_pkgs:
        row = f"    {pkg:16s}"
        for h in host_names:
            pkg_data = hosts[h]["packages"].get(pkg)
            if pkg_data:
                row += f"{fmt_ms(pkg_data['medianMs']):>14s}"
            else:
                row += f"{'--':>14s}"
        print(row)
    print()

    # Speedup summary if exactly haskell + java present.
    if "haskell" in hosts and "java" in hosts:
        print("  Java vs. Haskell (transform time, median):")
        for pkg in all_pkgs:
            hs = hosts["haskell"]["packages"].get(pkg)
            ja = hosts["java"]["packages"].get(pkg)
            if hs and ja and ja["medianMs"] > 0:
                ratio = hs["medianMs"] / ja["medianMs"]
                direction = "faster" if ratio > 1 else "slower"
                print(f"    {pkg:16s} Java is {abs(ratio):.2f}x {direction} than Haskell")
        print()
        setup_ratio = hosts["haskell"]["setupMs"] / max(hosts["java"]["setupMs"], 1)
        direction = "faster" if setup_ratio > 1 else "slower"
        print(f"  Setup: Java is {abs(setup_ratio):.2f}x {direction} than Haskell "
              f"({fmt_ms(hosts['java']['setupMs'])} vs {fmt_ms(hosts['haskell']['setupMs'])})")
    print("=" * 70)


if __name__ == "__main__":
    main()
