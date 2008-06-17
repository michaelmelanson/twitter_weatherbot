#!/bin/sh

erl -name weather -pz ebin -pz /usr/local/lib/yaws/ebin/ -s application start weather 