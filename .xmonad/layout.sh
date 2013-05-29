#!/bin/bash

case `keyboard -g` in
  "us(dvorak)" ) echo "US"
    ;;
  "ru" ) echo "RU"
    ;;
esac
