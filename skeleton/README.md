# Account Service

Service to create bank accounts.


## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/accounts


## Run locally using docker

This project comes with a docker container. It is built using 

```
docker build . -t accounts
```

in the root directory of the project. To run the docker container call
 
 ```
 docker run -p 8000:8000 -e "RELX_REPLACE_OS_VARS=true" -e "NODE_NAME=any_name" accounts
 ```
 
 Running with docker we are able to configure the node name of the erlang node
 using the `NODE_NAME` env var. To do so, relx must be informed that the 
 vm.args file contains env vars via `RELX_REPLACE_OS_VARS`.
 
 If the docker container is up and running, the web-frontend can be found at
 http://localhost:8000/accounts


## Testing

rebar3 & eunit are used for testing. To test the service use

```
rebar3 eunit
```

To test it within the docker container use

```
docker run accounts test
```


## Release

A release can be built using 

```
rebar3 release
```

