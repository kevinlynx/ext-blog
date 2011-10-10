#!/bin/sh
#This file will make a dist, and upload it to the remote server.
#Usage: upload-dist.sh user port path
#i.e: upload-dist.sh abc@codemacro.com 21 /home/test
user=$1
port=$2
path=$3
pack=ext-blog.tar.gz
./make-dist.sh
echo 'upload to ' $port $user:$path
scp -P $port $pack update-blog.sh $user:$path
ssh -p $port $user 

