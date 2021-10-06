#! /usr/bin/env bash

ACTION=$1
STORE=/tmp
declare -i STEP=1500
declare -i DEFVOL=20000
declare -i MAXVOLUME=90000

# reset, increase, decrease, mute, unmute, toggle
declare -i CURVOL=$DEFVOL
# Reads in the current volume
[[ -e $STORE/.volume ]] && CURVOL=`cat $STORE/.volume`

# Initialize at startup
if [[ $ACTION == "reset" && ! (-f $STORE/.volume || -f $STORE/.mute) ]]; then
  CURVOL=$DEFVOL
  echo $CURVOL > $STORE/.volume
  echo 0 > $STORE/.mute
  for i in `seq 0 6`; do
    pactl set-sink-mute $i 0 2>/dev/null
    pactl set-sink-volume $i $CURVOL 2>/dev/null
  done
  exit 0
fi

if [[ $ACTION == "read" ]]; then
  if [[ $(cat $STORE/.mute) == 0 ]]; then
    printf '%.0f%% \n' $(echo "$CURVOL / $MAXVOLUME * 100" | bc -l)  
  else
    echo Muted
  fi
  exit 0
fi

if [[ $ACTION == "set" ]]; then
  ACTION="unmute"
  CURVOL=$2
elif [[ $ACTION == "increase" ]]; then
  ACTION="unmute"
  CURVOL=$(($CURVOL + $STEP))
elif [[ $ACTION == "decrease" ]]; then
  CURVOL=$(($CURVOL - $STEP))
elif [[ $ACTION == "toggle" ]]; then
  if [[ `cat $STORE/.mute` -eq 1 ]]; then
    ACTION=unmute
  else
    ACTION=mute
  fi
fi

# If Volume went out of bounds correct it
if [[ $CURVOL -gt $MAXVOLUME ]]; then 
  CURVOL=$MAXVOLUME 
elif [[ $CURVOL -le 0 ]]; then
  CURVOL=0
  ACTION="mute"
fi

if [[ $ACTION == "mute" ]]; then
  for i in `seq 0 6`; do
    pactl set-sink-mute $i 1 2>/dev/null
  done
  echo 1 > $STORE/.mute
elif [[ $ACTION == "unmute" ]]; then
  for i in `seq 0 6`; do
    pactl set-sink-mute $i 0 2>/dev/null
  done
  echo 0 > $STORE/.mute
fi

for i in `seq 0 6`; do
  pactl set-sink-volume $i $CURVOL 2>/dev/null
done
# Write the new volume to disk to be read the next time the script is run.
echo $CURVOL > $STORE/.volume
