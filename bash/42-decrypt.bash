decrypt ()
{
  openssl enc -d -aes-256-cbc -md sha512 -pbkdf2 -iter 1000 -in "$1" -out "$1.dec"
}
