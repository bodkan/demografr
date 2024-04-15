import argparse

import msprime

parser = argparse.ArgumentParser()

# mandatory command-line arguments
parser.add_argument("--seq_len", type=float)
parser.add_argument("--rec_rate", type=float)
parser.add_argument("--mut_rate", type=float)
parser.add_argument("--output", type=str, required=True)

# model parameters
parser.add_argument("--Ne", type=float, required=True)

args = parser.parse_args()

ts = msprime.sim_ancestry(
  samples=round(args.Ne),
  population_size=args.Ne,
  sequence_length=args.seq_len,
  recombination_rate=args.rec_rate,
)

ts = msprime.sim_mutations(ts, args.mut_rate)

ts.dump(args.output)
