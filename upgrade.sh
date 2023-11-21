#!/bin/bash

release="/home/octa/orc/upgrade.tar.bz2"

if [[ -e $release ]]; then
    tar jxf $release -C /home/octa
    rm $release
fi
