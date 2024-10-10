import argparse

import msprime

parser = argparse.ArgumentParser()

# mandatory command-line argument
parser.add_argument("--path", type=str, required=True)

# model parameters
parser.add_argument("--Ne", type=float, required=True)

args = parser.parse_args()

ts = msprime.sim_ancestry(
  samples=round(args.Ne),
  population_size=args.Ne,
  sequence_length=1e6,
  recombination_rate=1e-8,
)

ts = msprime.sim_mutations(ts, 1e-8)

ts.dump(args.path + "/" + "result.trees")
