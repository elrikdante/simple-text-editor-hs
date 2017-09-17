#!/bin/sh
set -e -x
IMPORT=${IMPORT:-Common}
URL=${URL:-TODO: ADD A UNIVERSAL RECORD LOCATOR FOR}
IMPORT_TEMPLATE="${URL} ${IMPORT}"

cleanup() {
    echo "CLEANING UP"
    find . -iname "*.clean" -exec echo {} \+ -exec rm -v {} \;
}

mutate() {
    echo "UPDATE IMPORTS(${IMPORT}) to " ${IMPORT_TEMPLATE}   
    find . -iname "*.hs*" -exec echo {} \; \
	 -exec grep -i -n -e "import ${IMPORT}" {} \+ \
	 -exec sed -i.clean -E -e "s/.*import ${IMPORT}.*/import ${IMPORT} -- ${IMPORT_TEMPLATE//\//\\/}/" {} \;
}


cleanup
if [[ $1 == clean ]]; then
   exit 0
fi
mutate





