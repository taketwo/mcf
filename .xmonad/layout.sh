#!/bin/bash

case `$MCF/scripts/bin/keyboard -g` in
  "us(dvorak)" ) echo "DV"
    ;;
  "us" ) echo "US"
    ;;
  "ru" ) echo "RU"
    ;;
esac
