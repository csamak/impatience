#!/bin/bash
# Makes the root dir for the deployment container

mkdir -p root/bin && cp /bin/sh root/bin/
mkdir -p root/lib/x86_64-linux-gnu && cp /lib/x86_64-linux-gnu/{libnss_files.so.2,libnss_dns.so.2} root/lib/x86_64-linux-gnu/
mkdir -p root/lib64 && cp /lib64/ld-linux-x86-64.so.2 root/lib64/
mkdir -p root/etc && cp /etc/{protocols,services} root/etc/
mkdir -p root/usr/lib/x86_64-linux-gnu/gconv && cp /usr/lib/x86_64-linux-gnu/gconv/{UTF-16.so,UTF-32.so,UTF-7.so,gconv-modules,gconv-modules.cache} root/usr/lib/x86_64-linux-gnu/gconv/
