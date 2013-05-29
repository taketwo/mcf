#!/bin/sh

user="alexandrov88"
pass=""
new=`curl -u $user:$pass -s -sslreqd "https://mail.google.com/mail/feed/atom" | grep -c "<entry>"`
echo $new
