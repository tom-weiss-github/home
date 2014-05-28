from datetime import datetime, timedelta
import argparse

def todate(timestamp, delimiter):
    dt = datetime.utcfromtimestamp(timestamp // 1000000000)
    return dt.strftime("%Y{0}%m{0}%d".format(delimiter))

def totime(timestamp):
    seconds_since_epoch = timestamp // 1000000000
    nano_since_epoch = seconds_since_epoch * 1000000000
    nanos = timestamp - nano_since_epoch # recover just the nanoseconds
    dt = datetime.utcfromtimestamp(seconds_since_epoch)
    ts = dt.strftime('%H:%M:%S')
    ts += "."
    ts += str(nanos)
    return ts

parser = argparse.ArgumentParser()
parser.add_argument('-t', action='store', dest='time_sent')
args = parser.parse_args()
time_sent = int(args.time_sent)

print todate(time_sent, "/") + " " + totime(time_sent)
