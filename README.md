# Clojure Shell Scripting exercise using Babashka

Replicates functionality of this shell script:
```
export CURRENT_VERSION=4.4.17;
export CHANNEL_NAME=stable-4.5;
curl -sH 'Accept:application/json' "https://api.openshift.com/api/upgrades_info/v1/graph?channel=${CHANNEL_NAME}" | jq -r --arg CURRENT_VERSION "${CURRENT_VERSION}" '. as $graph | $graph.nodes | map(.version=='\"$CURRENT_VERSION\"') | index(true) as $orig | $graph.edges | map(select(.[0] == $orig)[1]) | map($graph.nodes[.].version) | sort_by(.)'
```

In essence, fetches OpenShift update paths for given version and
channel from REST API and utilizes
[Babashka](https://github.com/borkdude/babashka) and
[fzf](https://github.com/junegunn/fzf).

See [blog
posting](https://redhatnordicssa.github.io/shell-scripting-using-clojure)
for more background.

## Docker building

```
docker build -f Dockerfile -t openshift-available-updates-fetcher openshift-upgrade-path-fetcher .
docker tag openshift-available-updates-fetcher quay.io/tfriman/ocp-available-updates-fetcher
docker push quay.io/tfriman/ocp-available-updates-fetcher
```

## Running using docker

```
docker run -it --rm quay.io/tfriman/ocp-available-updates-fetcher:v1

docker run -it --rm quay.io/tfriman/ocp-available-updates-fetcher:v1 fast-4.5

docker run -it --rm quay.io/tfriman/ocp-available-updates-fetcher:v1 fast-4.5 4.4.17
```

## License

MIT
