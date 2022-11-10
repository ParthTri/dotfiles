#!/bin/bash 

type=$1
most_urgent_desc=`task rc.verbose: rc.report.$type.columns:description rc.report.$type.labels:1 limit:1 $type`
most_urgent_id=`task rc.verbose: rc.report.$type.columns:id rc.report.$type.labels:1 limit:1 $type`
most_urgent_due=`task rc.verbose: rc.report.$type.columns:due.relative rc.report.$type.labels:1 limit:1 $type`
echo "$most_urgent_id" > /tmp/tw_polybar_id
echo "$most_urgent_id · $most_urgent_desc · $most_urgent_due"
