#!/bin/bash

echo "wpa = wpa_supplicant.conf" >> /Volumes/GRISP/grisp.ini

cp wpa_supplicant.conf /Volumes/GRISP

diskutil unmount /Volumes/GRISP

