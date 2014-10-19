encrypt ()
{
  openssl aes-256-cbc -a -salt -in $1 -out $1.enc
}

decrypt ()
{
  openssl aes-256-cbc -a -d -in $1 -out $1.dec
}

