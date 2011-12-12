#!/bin/sh
screen -D -m -S ext-blog sbcl --dynamic-space 90 --eval "(load \"contrib/load-blog.lisp\")" 

