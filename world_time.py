# -*- mode: python -*-

import time
import re
import sys
import argparse
import os
from zoneinfo import ZoneInfo
from datetime import datetime

def print_time(location, a_time, dst="", delta=""):
    print("{0:12s} {1:02d}:{2:02d}:{3:02d} {4:02d}/{5:02d}/{6:04d} {7} {8}".format(
            location, a_time.hour, a_time.minute, a_time.second, a_time.month,
            a_time.day, a_time.year, delta, dst))

def print_tz_time(place, timezone, dst_info):
    now = datetime.now(ZoneInfo(timezone))
    my_tz = now.astimezone(ZoneInfo("America/Chicago")).utcoffset().total_seconds()
    yo_tz = now.astimezone(ZoneInfo(timezone)).utcoffset().total_seconds()
    delta = (yo_tz - my_tz) / 3600
    print_time(place, now, dst_info, delta)

if 1 == time.localtime().tm_isdst:
    is_dst_in_effect = True
else:
    is_dst_in_effect = False

print_tz_time("Chicago", "America/Chicago", "CDT" if is_dst_in_effect else "CST")
print_tz_time("New York", "America/New_York", "")
print_tz_time("Sao Paulo", "America/Sao_Paulo", "")
print_tz_time("UTC", "UTC", "")
print_tz_time("London", "Europe/London", "")
print_tz_time("Frankfurt", "Europe/Berlin", "")
print_tz_time("Madrid", "Europe/Madrid", "")
print_tz_time("Prague", "Europe/Prague", "")
print_tz_time("Athens", "Europe/Athens", "")
print_tz_time("Romania", "Europe/Bucharest", "")
print_tz_time("Johannesburg", "Africa/Johannesburg", "")
print_tz_time("Moscow", "Europe/Moscow", "")
print_tz_time("Istanbul", "Asia/Istanbul", "")
print_tz_time("India", "Asia/Calcutta", "")
print_tz_time("Bangkok", "Asia/Bangkok", "")
print_tz_time("Singapore", "Asia/Singapore", "")
print_tz_time("Hong Kong", "Asia/Hong_Kong", "")
print_tz_time("Seoul", "Asia/Seoul", "")
print_tz_time("Tokyo", "Asia/Tokyo", "")
print_tz_time("Sydney","Australia/Sydney", "")
