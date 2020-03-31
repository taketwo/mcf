encrypt() {
  openssl enc -aes-256-cbc -salt -md sha512 -pbkdf2 -iter 1000 -in "$1" -out "$1.enc"
}
