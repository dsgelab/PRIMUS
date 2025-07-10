import argparse

def probability(x):
    try:
        x = float(x)
    except ValueError:
        raise argparse.ArgumentTypeError("Not a float")
    if x < 0 or x > 1:
        raise argparse.ArgumentTypeError("Must be between 0 and 1")
    return x