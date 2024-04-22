FROM debian:latest
WORKDIR /home/colortester/colortest

RUN apt-get update && apt-get install -y doas && apt-get clean
RUN useradd -s /usr/bin/bash colortester
COPY .test_assets/doas.conf /etc
ADD .files.tar .
RUN chown -R colortester:colortester /home/colortester

USER colortester

CMD /usr/bin/bash -li
