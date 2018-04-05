#!/usr/bin/env python3

import os, sys
import subprocess
import time

powerLowAction = {
    5:  'poweroff',
    7:  'zenity --warning --text "Battery is only at 7%, will poweroff it at 5%"',
    8:  'zenity --warning --text "Battery is only at 8%, will poweroff it at 5%"',
    9:  'zenity --warning --text "Battery is only at 9%, will poweroff it at 5%"',
    10: None,
    20: None,
    70: None
}

powerHighAction = {
    5:  None,
    10: None,
    20: None,
    70: "notify-send 'charge up to 70%'"
}

def read_status():
    """
    This function reads the output of your command, finds the line with
    'percentage' (line 17, where first line = 0) and reads the figure
    """
    command = "upower -i $(upower -e | grep BAT) | grep --color=never -E percentage|xargs|cut -d' ' -f2|sed s/%//"
    get_batterydata = subprocess.Popen(["/bin/bash", "-c", command], stdout=subprocess.PIPE)
    return get_batterydata.communicate()[0].decode("utf-8").replace("\n", "")

def take_action():
    """
    When the charge is over 60% or below 40%, I assume the action does
    not have to be repeated every 10 seconds. As it is, it only runs
    1 time if charge exceeds the values. Then only if it exceeds the
    limit again.
    """
    # the two commands to run if charged over 80% or below 60%
    command_over         = "notify-send 'charged over %d%%'"
    command_below        = "notify-send 'charged below %d%%'"
    nofti_percent_factor = 10
    prev_charge          = int(read_status())

    while True:
        curr_charge = int(read_status())

        if curr_charge > prev_charge:
            if isinstance(powerHighAction[key], str):
                subprocess.Popen(["/bin/bash", "-c", powerHighAction[key]])
            elif curr_charge % nofti_percent_factor == 0:
                subprocess.Popen(["/bin/bash", "-c", command_hight % key ])
        elif curr_charge < prev_charge:
            if isinstance(powerHighAction[key], str):
                subprocess.Popen(["/bin/bash", "-c", powerLowAction[key]])
            elif curr_charge % nofti_percent_factor == 0:
                subprocess.Popen(["/bin/bash", "-c", command_low % key ])

        prev_charge = curr_charge

        time.sleep(10)


def main():
    """
    Implement -d option for daemon
    """
    fpid = os.fork()
    if fpid!=0:
    # Running as daemon now. PID is fpid
        sys.exit(0)

    take_action()

main()
