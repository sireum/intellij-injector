#!/bin/bash -e
VER=$(git log -n 1 --date=format:%Y%m%d --pretty=format:4.%cd.%h)
7z x sireum-injector.jar META-INF/plugin.xml
sed -i.bak "s/5.0.0-SNAPSHOT/${VER}/g" META-INF/plugin.xml
7z a sireum-injector.jar META-INF/plugin.xml
rm -fR META-INF
mv sireum-injector.jar intellij-injector.zip
echo $VER