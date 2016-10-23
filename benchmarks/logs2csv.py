#!/usr/bin/env python3

import re
import sys
from collections import defaultdict

cols = [
  'name', 'status', 'dom', 'size', 'MD', 'MC', 'PR', 'iterations', 'states',
  'time'
]

def parse(filename):
    stats = defaultdict(lambda: 'n/a') #namedtuple("Stats", ["MD", "MC", "PR"])
    heap = None
    def filter(line):
        nonlocal heap

        m = re.search("Multiple.*\((\w+)\).*:\s*(\d+)$", line)
        if m:
            stats[m.group(1)] = m.group(2)

        if not heap:
          m = re.search("Exit.Heap.(\w+)", line)
          if m:
              heap = m.group(1)

        if line.startswith("** Imprecision Exception"):
            heap = "Imprec"

        m = re.search('#.String.set.size.*\s(\d+)', line)
        if m:
            stats['size'] = m.group(1)

        if 0==0: #FIXME: How to deal with Imprecision Exceptions? heap != "Imprec":
          m = re.search("#.Fixpoint.iteration.*\s(\d+)$", line)
          if m:
              stats['iterations'] = m.group(1)

          m = re.search("Total.state.count.*\s(\d+)$", line)
          if m:
              stats['states'] = m.group(1)

          m = re.search("#.Analysis.took.", line)
          if m:
            stats['time'] = line[line.rfind(' ') + 1: -1]

          if re.search("time.out", line):
            stats['status'] = 'timeout'
        #else:
          #stats['iterations'] = stats['states'] = stats['time'] = 'n/a'

    with open(filename) as f:
        for line in f:
            filter(line.rstrip())

    if stats.get('status') == None:
        stats['status'] = heap or 'n/a'

    j = filename.rfind('_')
    i = filename.rfind('_', 0, j)
    stats['name'] = filename[filename.find('/') + 1 : i]
    stats['dom'] = filename[i + 1 : j]
    print('|'.join([stats[c] for c in cols]))


print('|'.join(cols))
for filename in sys.argv[1:]:
    parse(filename)
