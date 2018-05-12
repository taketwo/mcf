#!/bin/sh

TEMPFILE=`tempfile`".html"
markdown $1 > $TEMPFILE
see $TEMPFILE
