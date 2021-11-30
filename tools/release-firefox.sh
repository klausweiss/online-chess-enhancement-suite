#!/bin/sh

set -e

# https://addons.mozilla.org/en-US/developers/addon/api/key/
if [ -z ${WEB_EXT_API_KEY} ]; then echo WEB_EXT_API_KEY unset; exit 1; fi;
if [ -z ${WEB_EXT_API_SECRET} ]; then echo WEB_EXT_API_SECRET unset; exit 1; fi;


function getLatestVersionChanges {
	# keep in sync with release-github.sh
	cat CHANGELOG.md \
	| awk '
		BEGIN { headers = 0 } 
		{ 
			if (/^#/) { headers += 1 } 
			if (headers == 2 && !/^#/) { print } # only print lines of the second header
		}
	' \
	| grep -v "#ignore-in-changelog-within-extension" \
	| sed -e '/./,$!d' -e :a -e '/^\n*$/{$d;N;ba' -e '}'  
	# ^ remove empty lines from the beginning and the end of the file
}

function getLatestVersion {
	# keep in sync with release-github.sh
	cat CHANGELOG.md \
	| awk '
		BEGIN { headers = 0 } 
		{ 
			if (/^#/) { headers += 1 } 
			if (headers == 2) { print; exit; }
		}
	' \
		| sed 's/.*\[\(.*\)\].*/\1/'
}


VERSION=$(getLatestVersion)
getLatestVersionChanges > changelog-$VERSION.md

function releaseMozilla {
	npx web-ext-submit
}

echo "Copying changelog to clipboard"
xsel -ib < changelog-$VERSION.md
echo "Releasing mozilla plugin"
releaseMozilla
echo "Opening releases page"
firefox "https://addons.mozilla.org/en-US/developers/addon/online-chess-enhancement-suite/versions/"
