#!/bin/zsh

processes=(emacs firefox)

max_cpu=100
max_mem=10

foreach p ($processes)
{
    pid_max_cpu=$(command ps -eo user,pid,pcpu,pmem --no-headers | grep $p | awk '{ if ( $3 > '$max_cpu' ) print $2}')
    pid_max_mem=$(command ps -eo user,pid,pcpu,pmem --no-headers | grep $p | awk '{ if ( $4 > '$max_mem' ) print $2}')
    echo $p
    echo $pid_max_cpu
    echo $pid_max_mem
}
