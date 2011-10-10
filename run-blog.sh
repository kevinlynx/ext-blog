#!/bin/sh
screen -D -m -S ext-blog sbcl --eval "(load \"load-blog.lisp\")" --dynamic-space 90

