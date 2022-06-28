# Build stage 0
FROM erlang:22-alpine

# install git
RUN apk add --update git gcc g++ libc-dev

# Set working directory
RUN mkdir -p /buildroot/rebar3/bin
WORKDIR /buildroot

# Copy our Erlang test application
COPY . .

# And build the release
RUN rebar3 clean
RUN rm -rf _build
RUN rebar3 release


# Expose relevant ports
EXPOSE 8000
EXPOSE 8443
EXPOSE 4369

ENTRYPOINT ["/buildroot/entrypoint.sh"]

CMD ["deploy"]
