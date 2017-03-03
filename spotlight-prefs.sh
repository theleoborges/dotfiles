#!/bin/bash

sudo defaults delete /.Spotlight-V100/VolumeConfiguration Exclusions
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array-add '/Users/borgesle/workspace'
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array-add '/Users/borgesle/.m2'
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array-add '/Users/borgesle/.ivy2'
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array-add '/Users/borgesle/.npm'
sudo killall mds > /dev/null 2>&1
sleep 10
sudo mdutil -i on / > /dev/null
sudo mdutil -E / > /dev/null
