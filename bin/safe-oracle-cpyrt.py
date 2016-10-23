#!/usr/bin/env python3

import sys, os
import tempfile
import re

ORCL_CP_FILE = os.environ['HOME'] + "/safe.copyright.oracle.txt"
ORCL_CP_HEAD = "/*******************************************************************************"
ORCL_CP_TAIL = " ******************************************************************************/"

def printerr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def parse(filename, inject=False):
    cblocks = []
    rest = None
    temp = tempfile.NamedTemporaryFile(mode="r+")

    def printtmp(*args, **kwargs):
        print(*args, file=temp, **kwargs)

    with open(filename) as f:
        def cblock():
            def endline(line):
                return line.rstrip().endswith("*/")
            block = []
            for line in f:
                block.append(line)
                if endline(line):
                    break
            return block

        for line in f:
            if line.isspace():
                continue
            elif line.startswith("/*"):
                b = [line] + cblock()
                cblocks.append(b)
            else:
                printerr("end of header(s): " + line.rstrip())
                rest = line
                break

        for cb in cblocks:
            if any([re.match("[\*\s]+Copyright.*(KAIST|S-Core)", l) != None for l in cb]):
                print("KAIST (c) found")
            elif any([re.match("\s+Copyright.+2016.+Oracle", l) != None for l in cb]):
                print("Oracle (c) found, nothing to do")
                return
            else:
                print("Unknown (c) block:")
                print("".join(cb))


        if not inject:
            return

        assert(len(cblocks) <= 1)

        print("Injecting Oracle (c)")

        if len(cblocks) == 1:
            printtmp("".join(cblocks[0]), end="")
        with open(ORCL_CP_FILE) as inf:
            printtmp(ORCL_CP_HEAD)
            for l in inf:
                printtmp(l.rstrip())
            printtmp(ORCL_CP_TAIL)
        printtmp()
        printtmp(rest, end="")
        for l in f:
            printtmp(l, end="")

    temp.seek(0)

    printerr("Overwriting original file")

    with open(filename, "w") as wf:
        for line in temp:
            wf.write(line)

    temp.close()

        #cblocks.append([])
        #cblocks[-1].append(line)
        #m = re.search("Multiple.*\((\w+)\).*:\s*(\d+)$", line)
        #if m:
        #    stats[m.group(1)] = m.group(2)
        #m = re.search("Exit.Heap.(\w+)", line)
        #if m:
        #    heap = m.group(1)
        #if re.search("time.out", line):
        #    stats['fixpoint'] = 'timeout'





parse(sys.argv[1], True)
