#!/bin/bash 
sqlite3 "user-data" &
sbcl --script server.lisp

