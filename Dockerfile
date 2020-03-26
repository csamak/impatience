FROM scratch

WORKDIR /app

# System setup
COPY /bin/sh /bin/
COPY /lib/x86_64-linux-gnu/libnss_files.so.2 /lib/x86_64-linux-gnu/libnss_dns.so.2 /lib/x86_64-linux-gnu/
COPY /lib64/ld-linux-x86-64.so.2 /lib64/
COPY /etc/protocols /etc/services /etc/
COPY /usr/lib/x86_64-linux-gnu/gconv/UTF-16.so /usr/lib/x86_64-linux-gnu/gconv/UTF-32.so /usr/lib/x86_64-linux-gnu/gconv/UTF-7.so /usr/lib/x86_64-linux-gnu/gconv/gconv-modules /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache /usr/lib/x86_64-linux-gnu/gconv/

# Specific deps
COPY ./bin/lib/. /lib/x86_64-linux-gnu/
COPY ./impatience.dhall /app/impatience.dhall
COPY ./bin/impatience-exe /app/impatience-exe
ENV PATH="/app:${PATH}"
