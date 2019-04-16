#!/bin/sh
# Stop old server running on giscours:

WEBSERVER=giscours.informatik.uni-kiel.de
CURRYHOME=/opt/pakcs/pakcs

ssh $WEBSERVER $HOME/.cpm/bin/curry-cgi stopscript $HOME/public_html/SAM/recipes/spicey.cgi.server
