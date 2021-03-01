set -ex

stack install \
  --file-watch --watch-all \
  --pedantic --fast \
  --exec='upcheck ./examples/cs-syd.yaml'
