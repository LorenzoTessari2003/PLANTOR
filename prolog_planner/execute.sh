#!/usr/bin/env bash

cd $1

# Append first argument of script to end of includes_original.pl file and save it to includes.pl
echo ":- include('$2')." > includes.pl

# Run the prolog script
swipl -l planner.pl -t "time(plan($3))"

