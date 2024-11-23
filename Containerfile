# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: CC0-1.0

FROM debian:latest
WORKDIR /home/colortester/colortest

RUN apt-get update && apt-get install -y doas && apt-get clean
RUN useradd -s /usr/bin/bash colortester
COPY .test_assets/doas.conf /etc
USER colortester
ADD .files.tar .
RUN doas chown -R colortester:colortester /home/colortester

CMD /usr/bin/bash -li
