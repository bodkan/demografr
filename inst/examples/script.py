import argparse

import msprime

parser = argparse.ArgumentParser(description="Simple single population simulation")

# mandatory command-line arguments
parser.add_argument("--sequence_length", type=float, required=True, help="Sequence length")
parser.add_argument("--recombination_rate", type=float, required=True, help="Recombination rate")
parser.add_argument("--output_path", type=str, required=True, help="Path to an output tree sequence")

# model parameters
parser.add_argument("--Ne", type=float, required=True)

args = parser.parse_args()

ts = msprime.sim_ancestry(
    25,
    population_size=args.Ne,
    sequence_length=args.sequence_length,
    recombination_rate=args.recombination_rate,
)

ts.dump(args.output_path)
