#!/bin/bash
while true
do
    BAT=`acpi -b | awk '{gsub(/%,/,""); print $4}' | sed 's/%//g'`
    STATUS=`acpi -b | awk '{gsub(/,/,""); print $3}'`
    if [[ $BAT -lt 10  ]]; then
        if [[ $STATUS = "Discharging" ]]; then
	    notify-send "Warning, battery level below 10 percent" -u critical -i "notification-power"
            sleep 30
        fi
    fi
    sleep 1
done
