#!/bin/sh
#This file will be uploaded to remote server, and you can ssh to the server
#, and execute this file to tar the file, resume the lisp screen.
tar xvf ext-blog.tar.gz 
screen -r ext-blog

