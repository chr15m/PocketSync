# This Makefile is for native builds via Cordova

# Android notes:
# Will need to set Java environment to at least 11:
#   sudo apt install openjdk-11-jre openjdk-11-jre-headless
#   export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
#   export PATH=$JAVA_HOME/bin:$PATH
# Had to upgrade Android command line tools:
#   Download from here: https://developer.android.com/studio#command-tools for Linux
#   Unzip and move cmdline-tools to ~/bin/contrib/android_sdk/cmdline-tools
#   Install the build-tools latest:
#   ~/bin/contrib/android_sdk$ ./cmdline-tools/latest/bin/sdkmanager "build-tools;30.0.3" --verbose --sdk_root=~/bin/contrib/android_sdk

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
	@echo "  make android-binary    # to make a .aab that can be uploaded to the Play Store"
	@echo "  make ios-start         # to open XCode with a project you can run to deploy"
	@echo "  make ios-run           # run on the device (once working in XCode)"

GFX=build/resources/icon.png build/resources/splash.jpg build/icon.png
JS=build/www/js/main.js
CORDOVA=cd build && ../../node_modules/.bin/cordova
APK=build/platforms/android/app/build/outputs/bundle/release/app-release.aab
REV=$(shell git rev-parse HEAD | cut -b-8)

android: build/platforms/android/android.json

android-prepare: build/resources/android

build/resources/android: $(GFX)
	$(CORDOVA)-res android --copy
	$(CORDOVA) prepare android

android-install: android $(JS) build/resources/android pocketsync.keystore
	@adb devices | grep -oE 'device$$' || ( echo "No device attached. Aborting build."; exit 1 )
	make -C ..
	$(CORDOVA) run android

android-binary: posync-release-$(REV).aab

posync-release-$(REV).aab: android $(JS) build/resources/android
	# cp gradle.properties platforms/android/app/gradle.properties
	@echo "Running cordova build --release"
	@$(CORDOVA) build android --release -- --packageType=bundle --keystore=../pocketsync.keystore --storePassword="${KEYSTORE_PASSWORD}" --alias="posync" --password="${KEYSTORE_PASSWORD}"
	mv ${APK} $@

ios: build/platforms/ios/ios.json

ios-start: ios $(JS) $(GFX)
	make -C ..
	$(CORDOVA)-res ios --copy
	$(CORDOVA) prepare ios
	$(CORDOVA) build ios --codeSigningIdentity="Apple Development" ||:
	@echo "Launching XCode, please wait."
	@echo "Next steps:"
	@echo "* Select the right sigining settings (follow the errors)"
	sleep 5
	open build/platforms/ios/*.xcworkspace

ios-run: ios $(JS) $(GFX)
	make -C ..
	security unlock-keychain
	$(CORDOVA) prepare ios
	$(CORDOVA) run ios

pocketsync.keystore:
	@echo "Generating a new Android keystore but you might need to copy the existing one."
	keytool -genkey -v -keystore pocketsync.keystore -alias posync -keyalg RSA -keysize 2048 -validity 1000

build/platforms/android/android.json: build/package.json
	[ -f "$@" ] || ( $(CORDOVA) platform add android )
	$(CORDOVA) plugin add https://github.com/MCluck90/cordova-plugin-android-volume
	rm -f build/config.xml && cd build && ln -s ../config.xml && touch config.xml
	touch $@

build/platforms/ios/ios.json: build/package.json build/config.xml build/resources/ios
	[ -f "$@" ] || ( $(CORDOVA) platform add ios )
	$(CORDOVA) plugin add https://github.com/chr15m/cordova-plugin-get-volume
	$(CORDOVA) plugin add https://github.com/clemdesign/cordova-plugin-ios-disable-bounce
	$(CORDOVA) plugin add cordova-plugin-statusbar
	$(CORDOVA) plugin add cordova-plugin-screen-orientation
	touch $@

build/www/js/main.js: ../build/index.html
	rm -rf build/www && cd build && ln -s ../../build www && touch www/js/main.js
	touch ../build/index.html

../build/index.html:
	make -C ..
	touch ../build
	touch ../build/index.html

build/package.json build: ../build/index.html
	mkdir -p build
	[ -f "build/package.json" ] || npx cordova create build cx.mccormick.pocketsync 'PO Sync'
	touch build/package.json

build/config.xml: build
	rm -f build/config.xml && cd build && ln -s ../config.xml && touch config.xml

build/icon.png: ../app-icon.svg
	mkdir -p build
	inkscape --export-png=$@ --export-width=512 --export-height=512 $<

build/resources/icon.png: ../app-icon.svg
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=1024 --export-height=1024 $<

build/resources/splash.jpg: ../splash.svg
	mkdir -p build/resources
	inkscape --export-png=$@ --export-width=2732 --export-height=2732 $<

.PHONY: clean

clean:
	rm -rf build posync-release.aab pocketsync.keystore
