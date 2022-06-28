#!/bin/sh

export RELX_REPLACE_OS_VARS=true

case "$1" in
    deploy)
        echo mycookie > $HOME/.erlang.cookie
        chmod 400 $HOME/.erlang.cookie
        ./_build/default/rel/erlbank_flex_accounts/bin/erlbank_flex_accounts foreground
        ;;
    test)
        rebar3 eunit
        ;;
    deploy-with-elk)

        if ! [ -x "$(command -v curl)" ]; then
            apk add curl
        fi

        # Wait for kibana
        while true; do # wait for local kibana
            echo "$(date) waiting kibana ..."
            curl -s -XGET "$KIBANA_HOST:5601"
            if [ "$?" -eq 0 ]; then
                break
            fi
            sleep 10
        done

        # Create default index
        echo "Creating kibana index..."
        curl -XPOST -H 'Content-Type: application/json' \
             -H 'kbn-xsrf: anything' \
             "http://$KIBANA_HOST:5601/api/saved_objects/index-pattern/erlbank-*" \
             '-d{"attributes":{"title":"erlbank-*","timeFieldName":"@timestamp"}}'

        # Wait for logstash
        while true; do # wait for local logstash
            echo "$(date) waiting logstash ..."
            curl -s -XGET "$LOGSTASH_HOST:9600"
            if [ "$?" -eq 0 ]; then
                break
            fi
            sleep 10
        done

        sleep 10

        echo mycookie > $HOME/.erlang.cookie
        chmod 400 $HOME/.erlang.cookie
        ./_build/default/rel/erlbank_flex_accounts/bin/erlbank_flex_accounts foreground
        ;;
    *)
        sh -c $@
        ;;
esac
