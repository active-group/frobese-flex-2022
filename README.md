# FLEX-Schulung bei frobese 2022

## Auschecken des Projekts:

    git clone git@github.com:active-group/frobese-flex-2022.git
    git submodule update --init

Das checkt auch die Teilprojekte als "git submodules" aus.

## Bauen der individuellen Services

    cd <accounts|transfers|bank_statements>
    rebar3 clean
    rm -rf _build
	rebar3 release
	
## Starten der inviduellen Services

    cd accounts
    ./entrypoint.sh deploy
	
	cd transfers
    env ACCOUNTS_HOST=<hostname> ./entrypoint.sh deploy
	
	cd bank_statements
    env ACCOUNTS_HOST=<hostname> TRANSFERS_HOST=<hostname> ./entrypoint.sh deploy

Die Webseiten sind dann auf `localhost:8000`, `localhost:8001` und `localhost:8002`.

## Gesamt-Deployment mit Docker

    cd deployment
	./deploy.sh
	
Oder alternativ die beiden Befehle, die in `deploy.sh` drinstehen:

    docker-compose build
    docker-compose up

Achtung: Das dauert ein paar Minuten.

Die Gesamt-UI ist dann auf:

	http://localhost:8080/
	
Das Kibana läuft hier:

    http://localhost:5601/
	
Um da was zu machen:

* "set up index patterns": `erlbank-*`
* "time field name": `@timestamp`
* auf den Stern klicken, "set as default index"
* Dann auf `Discover` rechts klicken

