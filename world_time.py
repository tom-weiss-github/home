import datetime
import time
import re
import sys
import argparse


def print_time(location, a_time, dst="", delta=""):
    print("{0:12s} {1:02d}:{2:02d}:{3:02d} {4:02d}/{5:02d}/{6:04d} {7}{8}".format(
            location, a_time.hour, a_time.minute, a_time.second, a_time.month,
            a_time.day, a_time.year, dst, delta))



descr = "\nworld_time.py [-d DD/MM/YYYY] [-t HH:MM]" + \
        "\n\nThis script script prints the current time in various timezones of" + \
        "\ninterest to TT software users.  With no arguments the current time is used." + \
        "\nIf a date and time are passed in, they are used."

parser = argparse.ArgumentParser(description=descr)
parser.add_argument("-d", "--other-date",
                    action="store", dest="other_date", default=None,
                    help="(Optional) Print current world times based on this date.")

parser.add_argument("-t", "--other-time", default=None,
                    action="store", dest="other_time",
                    help="(Optional) Print current world times based on this time.")


args = parser.parse_args()

now = datetime.datetime.now()

if(None != args.other_date and
   None != args.other_time):

    time_regex = re.compile(r"[0-9]{2}:[0-9]{2}")
    date_regex = re.compile(r"[0-9]{2}/[0-9]{2}/[0-9]{4}")

    if(None == time_regex.match(args.other_time)):
        print("Error, the time, '{0}', must be HH:MM.".format(args.other_time))
        sys.exit(0)

    if(None == date_regex.match(args.other_date)):
        print("Error, the date, '{0}', must be DD/MM/YYYY.".format(args.other_date))
        sys.exit(0)

    year = int(((args.other_date).split('/'))[2])
    month = int(((args.other_date).split('/'))[1])
    day = int(((args.other_date).split('/'))[0])
    hour = int(((args.other_time).split(':'))[0])
    minute = int(((args.other_time).split(':'))[1])
    second = 0

    now = datetime.datetime(year, month, day, hour, minute, second)

is_dst_in_effect = False
new_jersey_offset  = 1
singapore_offset = 0   # Singapore is +13 hours during CDT and +14 hours in CST.
japan_offset     = 0   # Japan     is +14 hours during CDT and +15 hours in CST.
hongkong_offset = 0   # Hong Kong is +13 hours during CDT and +14 hours in CST.
if(1 == time.localtime().tm_isdst):
    is_dst_in_effect = True
    singapore_offset = 13
    japan_offset     = 14
    hongkong_offset = 13
else:
    is_dst_in_effect = False
    singapore_offset = 14
    japan_offset     = 15
    hongkong_offset = 14

singapore_delta = datetime.timedelta(days=0, seconds=(singapore_offset*60*60))
singapore_now = now + singapore_delta

hongkong_delta = datetime.timedelta(days=0, seconds=(hongkong_offset*60*60))
hongkong_now = now + hongkong_delta

japan_delta = datetime.timedelta(days=0, seconds=(japan_offset*60*60))
japan_now = now + japan_delta

nj_delta = datetime.timedelta(days=0, seconds=(new_jersey_offset*60*60))
nj_now = now + nj_delta


print_time("Chicago", now, "CDT" if is_dst_in_effect else "CST", "")
print_time("New Jersey", nj_now, "", "+" + str(new_jersey_offset))
print_time("Singapore", singapore_now, "", "+" + str(singapore_offset))
print_time("Hong Kong", hongkong_now, "", "+" + str(hongkong_offset))
print_time("Japan", japan_now, "", "+" + str(japan_offset))
