#!/bin/bash

case `keyboard -g` in
  "us(dvorak)" ) echo "DV"
    ;;
  "us" ) echo "US"
    ;;
  "ru" ) echo "RU"
    ;;
esac
