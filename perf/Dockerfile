# syntax=docker/dockerfile:1.7
#
# Environment for perf/nlperf: the official NetLogo 7.0.4 release, run on the
# Java runtime it bundles (17.0.2), plus a JDK for compiling the harness and
# reading profiles. The repo is bind-mounted at run time, never copied in.

FROM azul/zulu-openjdk:21.0.2@sha256:8790aac9a209406770015f77f983ee0dc9ab21f34414bf5e522eda7fcfa0e54d AS netlogo
ADD --checksum=sha256:75636b8469ea4995ec051cfbc91e525f171be84b8f959b86bc080fbff702dfbc \
    https://downloads.netlogo.org/7.0.4/NetLogo-7.0.4-64.tgz /tmp/netlogo.tgz
# Only what headless runs need: the jars, the bundled runtime, the bundled extensions.
RUN mkdir /opt/netlogo \
 && tar -xzf /tmp/netlogo.tgz -C /opt/netlogo --strip-components=1 \
      "NetLogo 7.0.4/lib/app" "NetLogo 7.0.4/lib/runtime" "NetLogo 7.0.4/extensions"

FROM azul/zulu-openjdk:21.0.2@sha256:8790aac9a209406770015f77f983ee0dc9ab21f34414bf5e522eda7fcfa0e54d
COPY --from=netlogo /opt/netlogo /opt/netlogo

# Run as the host user so files written into the bind-mounted repo stay host-owned.
ARG UID=1000
ARG GID=1000
RUN groupadd -o -g "$GID" nlperf \
 && useradd -o -m -d /home/nlperf -u "$UID" -g "$GID" -s /bin/bash nlperf
USER nlperf
ENV HOME=/home/nlperf
