#! /usr/bin/env bash

SOUNDFILE="/tmp/.soundout"

if [ -z "$1" ]; then
  echo "Usage: $0 <sinkId/sinkName>" >&2
  echo "Valid sinks:" >&2
  pactl list short sinks >&2
  exit 1
fi

if [[ $1 == "init" || $1 == "Init" ]]; then
  [[ -f $SOUNDFILE ]] || echo Speakers > $SOUNDFILE
  exit 0
fi

if [[ $1 == "read" || $1 == "Read" ]]; then
  cat $SOUNDFILE
  exit 0
fi 

newSink="$1"
if [[ $newSink == "toggle" || $newSink == "Toggle" ]]; then
  current=$(cat $SOUNDFILE)
  if [[ $current == "Headphones" ]]; then
    newSink="Speakers"
  elif [[ $current == "Speakers" ]]; then
    newSink="Headphones"
  else
    exit 127
  fi
fi
if [[ $newSink == "headphones" || $newSink == "Headphones" ]]; then
  newSink="Razer"
  echo "Headphones" > $SOUNDFILE
elif [[ $newSink == "speakers" || $newSink == "Speakers" ]]; then
  newSink="USB_AUDIO"
  echo "Speakers" > $SOUNDFILE
fi
echo NewSink: $newSink
sinkNum=$(pactl list short sinks | awk -v var="$newSink" 'index($2, var) {print $1; exit 0}')
sinkName=$(pactl list short sinks | awk -v var="$newSink" 'index($2, var) {print $2; exit 0}')
echo SN: $sinkNum
pactl set-default-sink "$sinkName"

pactl list short sink-inputs | while read stream; do
streamId=$(echo $stream | cut '-d ' -f1)
pactl move-sink-input "$streamId" "$sinkNum"
done
# vim: ft=sh
