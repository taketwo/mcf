#!/bin/bash

source /home/sergey/.mcf/bash/archives.bash

mkdir /tmp/kebrum
cd /tmp/kebrum
wget https://en.kebrum.com/webclient/config.zip
extract config.zip
mv ssl /etc/openvpn/
for config in *TCP*; do
  echo "auth-user-pass /home/sergey/.kebrum" >> "$config"
  IFS='.' read -ra tokens <<< "$config"
  name="${tokens// /}.Kebrum.TCP.conf"
  mv "$config" /etc/openvpn/$name
done
cd ..
rm -rf kebrum
