#!/bin/bash

PS4='$LINENO: '
set -x

rm -rf ~/.emacs.d/diary

WGET=/usr/bin/wget

mkdir -p /tmp/calimport
cd /tmp/calimport

CALURL=( \
"<https://calendar.google.com/calendar/ical/blahblah>" \
"<https://calendar.google.com/calendar/ical/blahblah>" )

ICSNAME=( cal-1.ics cal-2.ics )

for ((i=0;i<${#CALURL[@]};i++))
do
$WGET -O ${ICSNAME[$i]} ${CALURL[$i]};
done

emacs --batch -l ics-diary.el

# Clean up
rm -rf /tmp/calimport
