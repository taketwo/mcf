#!/bin/bash

DEST="/etc/openvpn"

# Remove previous istallation
cd $DEST
rm *Kebrum*conf
rm -rf ssl/{ta.key,ca.crt,client.crt,client.key}

# Download and install new files
mkdir /tmp/kebrum
cd /tmp/kebrum
wget https://en.kebrum.com/webclient/config.zip
unzip config.zip
mv ssl $DEST
for config in *TCP*; do
  echo "auth-user-pass /home/sergey/.kebrum" >> "$config"
  IFS='.' read -ra tokens <<< "$config"
  name="${tokens// /}.Kebrum.TCP.conf"
  mv "$config" $DEST/$name
done
cd ..
rm -rf kebrum
