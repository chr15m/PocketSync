# This Makefile is for native builds via Cordova

# Android notes:
# Had to upgrade Android command line tools:
# Download from here: https://developer.android.com/studio#command-tools for Linux
# Unzip and move cmdline-tools to ~/bin/contrib/android_sdk/cmdline-tools
# Install the build-tools latest:
# chrism@embless:~/bin/contrib/android_sdk$ ./cmdline-tools/bin/sdkmanager "build-tools;30.0.3" --verbose --sdk_root=~/bin/contrib/android_sdk

# iOS notes:
# If you can't Run in Xcode make sure the correct "scheme" is selected
# Under "Product -> Scheme" menu and select the app itself
# You will have to select yourself in the signing (red error will lead you)
# When prompted for password choose to always allow.

help:
	@echo "Make target options:"
	@echo "  make android           # to set up for android builds"
	@echo "  make ios               # to set up for ios builds"
	@echo "  make android-install   # to deploy an android dev build to the device"
	@echo "  make ios-start         # to open XCode with a project you can run to deploy"

GFX=build/resources/icon.png build/resources/splash.jpg build/icon.png
JS=build/www/js/main.js
CORDOVA=cd build && ../../node_modules/.bin/cordova

android: build/platforms/android/android.json

android-install: android $(JS) $(GFX)
	make -C ..
	$(CORDOVA) run android

ios: build/platforms/ios/ios.json

ios-start: ios $(JS) $(GFX)
	make -C ..
	# security unlock-keychain -p YOUR_LOGIN_PASSWORD_HERE login.keychain
	# security unlock-keychain
	cd build && ../../node_modules/.bin/cordova-res ios --copy
	$(CORDOVA) prepare ios
	$(CORDOVA) build ios --buildFlag="-scheme PocketSync" --codeSigningIdentity="Apple Development" ||:
	@echo "Launching XCode, please wait."
	@echo "Next steps:"
	@echo "* Select the PocketSync scheme"
	@echo "* Select the right sigining settings (follow the errors)"
	sleep 5
	open build/platforms/ios/*.xcworkspace

build/platforms/android/android.json: build/package.json build/config.xml
	[ -f "$@" ] || ( $(CORDOVA) platform add android )
	$(CORDOVA) plugin add https://github.com/MCluck90/cordova-plugin-android-volume
	touch $@

build/platforms/ios/ios.json: build/package.json build/config.xml
	[ -f "$@" ] || ( $(CORDOVA) platform add ios )
	$(CORDOVA) plugin add https://github.com/chr15m/cordova-plugin-get-volume
	$(CORDOVA) plugin add https://github.com/clemdesign/cordova-plugin-ios-disable-bounce
	$(CORDOVA) plugin add cordova-plugin-statusbar
	touch $@

build/www/js/main.js: ../build/index.html
	rm -rf build/www && cd build && ln -s ../../build www && touch www/js/main.js
	touch ../build/index.html

build/config.xml: build
	rm -f build/config.xml && cd build && ln ../config.xml && touch config.xml

../build/index.html:
	make -C ..
	touch ../build
	touch ../build/index.html

build/package.json: ../build/index.html
	mkdir -p build
	npx cordova create build cx.mccormick.pocketsync PocketSync

build/icon.png: ../app-icon.svg
	mkdir -p build
	inkscape --export-png=$@ --export-width=512 --export-height=512 $<

build/resources/icon.png: ../app-icon.svg
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=1024 --export-height=1024 $<

build/resources/splash.jpg: ../splash.svg
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=2732 --export-height=2732 $<
