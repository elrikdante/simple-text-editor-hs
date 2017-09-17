set -e -x
IMPORT=${IMPORT:-Common}
URL=${URL:-TODO: ADD A UNIVERSAL RECORD LOCATOR FOR}
find . -iname "*.hs*" -exec echo {} \; -exec grep -i -n -e "import ${IMPORT}" {} \+ -exec sed -i.clean -E -e 's/.*import ${IMPORT}.*/import ${IMPORT} -- ${URL} ${IMPORT}/' {} \;
