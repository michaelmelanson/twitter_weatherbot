#!/bin/sh

erl -sname weather -pz ebin -pz /usr/local/lib/yaws/ebin/ -pz /usr/lib/yaws/ebin/ -s application start weather 
