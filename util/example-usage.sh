#!/bin/sh

update-toggl-entries \
    --trace \
    -a your@email.address \
    -t "${TOGGL_TOKEN?}" \
    -w 1234567 \
    -q client_ids=12345678,23456789 \
    -e 2019-01-01 \
    -u 13 \
    -o entries.json
