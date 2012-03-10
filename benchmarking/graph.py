#!/usr/bin/env python

import sys
from scipy.stats import stats
import Gnuplot, Gnuplot.funcutils

port_map = {"80": "Apache", "81": "lighttpd", "8080": "HeMan"}

def process_file(file_name):
    _, it, size, conns, port = file_name.split("_")
    server = port_map[port]
    lines = [line.strip() for line in open(file_name)]
    finished = lines[-4]
    parts = finished.split(" ")
    time = "%d.%0.3d%0.3d" % (int(parts[2]), int(parts[4]), int(parts[7]))

    requests = lines[-3]
    parts = requests.split(" ")
    successes = parts[7]
    rate = float(successes)/float(time)

    return (server, size, int(conns), it, time, rate)

def map_add(m, key, x):
    if key not in m:
        m[key] = []
    m[key].append(x)

def organize_data(data, size):
    data = [x[:1]+x[2:] for x in data if x[1] == size]
    servers = {x[0]: [] for x in data}
    for x in data:
        servers[x[0]].append(x[1:])
    for v in servers.values():
        v.sort()
    return servers

def produce_stats(data):
    m = {}
    for (server, size, conns, it, time, rate) in data:
        map_add(m, (server, size, conns), rate)
    data = []
    for k, v in m.items():
        mean = stats.nanmean(v)
        stddev = stats.nanstd(v)
        data += [k + (mean, stddev)]
    return data

def graph(size, table):

    g = Gnuplot.Gnuplot(debug=1)
    g.title(size)
    g('set style data errorlines')
    g('set yrange [0:]')
    g.xlabel('# of Concurrent requests')
    g.ylabel('Requests/second')

    lines = []
    for server, data in table.items():
        xs, ys, stds = zip(*data)
        lines.append(Gnuplot.Data(xs, ys, stds, title=server))
    g.replot(*lines)

    raw_input('Please press return to continue...\n')


def main(args):
    data = [process_file(f) for f in args[1:]]
    data = produce_stats(data)

    sizes = set(x[1] for x in data)

    tables = {size: organize_data(data, size) for size in sizes}
    size = '16K'
    graph(size, tables[size])

if __name__ == '__main__':
    sys.exit(main(sys.argv))
