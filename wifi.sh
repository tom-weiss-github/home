#!/bin/bash

watch -n 1 'nmcli device wifi list | grep -i tphone; nmcli device wifi rescan'
