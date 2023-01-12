# This Makefile is for native builds via Cordova

# Android notes:
# Had to upgrade Android command line tools:
# Download from here: https://developer.android.com/studio#command-tools for Linux
# Unzip and move cmdline-tools to ~/bin/contrib/android_sdk/cmdline-tools
# Install the build-tools latest:
# chrism@embless:~/bin/contrib/android_sdk$ ./cmdline-tools/bin/sdkmanager "build-tools;30.0.3" --verbose --sdk_root=~/bin/contrib/android_sdk

# iOS notes:
# If you can't Run in Xcode make sure the correct "scheme" is selected

all: build build/icon.png build/resources/icon.png build/resources/splash.jpg

CORDOVA=cd build && npx cordova

build: ../build
	mkdir -p build
	npx cordova create build cx.mccormick.pocketsync PocketSync
	$(CORDOVA) platform add ios
	$(CORDOVA) platform add android
	$(CORDOVA) plugin add https://github.com/MCluck90/cordova-plugin-android-volume
	$(CORDOVA) plugin add https://github.com/chr15m/cordova-plugin-get-volume
	rm -rf build/www && cd build && ln -s ../../build www
	rm -f build/config.xml && cd build && ln -s ../config.xml
	touch build

../build:
	make -C ..

build/icon.png: ../app-icon.svg build
	inkscape --export-png=$@ --export-width=512 --export-height=512 $<

build/resources/icon.png: ../app-icon.svg build
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=1024 --export-height=1024 $<

build/resources/splash.jpg: ../splash.svg build
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=2732 --export-height=2732 $<

install-android:
	$(CORDOVA) run android

install-ios:
	$(CORDOVA) cordova-res ios --copy
	$(CORDOVA) run ios
