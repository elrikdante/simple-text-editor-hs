#!/bin/sh
set -e -x
IMPORT=${IMPORT:-Common}
URL=${URL:-TODO: ADD A UNIVERSAL RECORD LOCATOR FOR}
IMPORT_TEMPLATE="${URL} ${IMPORT}"

echo "CLEANING UP"
find . -iname "*.clean" -exec echo {} \+ -exec rm -v {} \;

echo "UPDATE IMPORTS(${IMPORT}) to " ${IMPORT_TEMPLATE}
exit 0

find . -iname "*.hs*" -exec echo {} \; \
  -exec grep -i -n -e "import ${IMPORT}" {} \+ \
  -exec sed -i.clean -E -e "s/.*import ${IMPORT}.*/import ${IMPORT} -- ${IMPORT_TEMPLATE//\//\\/}/" {} \;
