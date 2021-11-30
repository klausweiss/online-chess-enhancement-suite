#!/bin/sh

set -e


function getLatestVersionChanges {
	cat CHANGELOG.md \
	| awk '
		BEGIN { headers = 0 } 
		{ 
			if (/^#/) { headers += 1 } 
			if (headers == 2 && !/^#/) { print } # only print lines of the second header
		}
	' \
	| sed -e '/./,$!d' -e :a -e '/^\n*$/{$d;N;ba' -e '}'  
	# ^ remove empty lines from the beginning and the end of the file
}

function getLatestVersion {
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

function releaseGithub {
	gh release create \
		$VERSION \
		--title $VERSION \
		--notes-file changelog-$VERSION.md \
		chrome_online-chess-enhancement-suite_$VERSION.zip \
		./online_chess_enhancement_suite-$VERSION-*fx.xpi
}

echo "Releasing plugin on github"
releaseGithub
