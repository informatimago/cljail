#!/bin/sh -x

PKGROOT=`pwd`
DMGNAME=chroot.dmg

VOLNAME=10-4-chroot
sudo whoami

## A regular Mac OS X 10.4 and Xcode 2.4 installation occupies around 4 GB
## (not including any space for user files or temporaries, so add 512 MB...)

if [ ! -r $DMGNAME ]; then
hdiutil create -fs HFS+ -volname $VOLNAME -verbose -size 4.5g $DMGNAME
fi

## You may want to create a disk image with a HFSX or UFS filesystem instead
## for work that requires a case sensitive filesystem.
# hdiutil create -fs HFSX -volname $VOLNAME -verbose -size 5g $DMGNAME
# hdiutil create -fs UFS -volname $VOLNAME -verbose -size 5g $DMGNAME


hdiutil mount $DMGNAME
cd /Volumes/$VOLNAME || exit 1

## Installing the packages into a non-bootable Volume or directory that is
## not / can only be done if the CM_BUILD environment variable is set. 
CM_BUILD=CM_BUILD
export CM_BUILD


## Mac OS X 10.4.0 or 10.4.4 install

sudo installer -verbose -pkg $PKGROOT/BaseSystem.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/Essentials.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/BSD.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/X11User.pkg -target /Volumes/$VOLNAME

## Mac OS X 10.4.8 or 10.4.9 update

if [ "`arch`" = "ppc" ]; then
test -r $PKGROOT/MacOSXUpdCombo10.4.8PPC.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.8PPC.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-002Ti.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-002Ti.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSXUpdCombo10.4.9PPC.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.9PPC.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-005Ti.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-005Ti.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSXUpdCombo10.4.10PPC.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.10PPC.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-006Ti.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-006Ti.pkg -target /Volumes/$VOLNAME
fi
if [ "`arch`" = "i386" ]; then
test -r $PKGROOT/MacOSXUpdCombo10.4.8Intel.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.8Intel.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-002Univ.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-002Univ.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSXUpdCombo10.4.9Intel.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.9Intel.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-005Univ.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-005Univ.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSXUpdCombo10.4.10Intel.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSXUpdCombo10.4.10Intel.pkg -target /Volumes/$VOLNAME
test -r $PKGROOT/SecUpd2007-006Univ.pkg && \
sudo installer -verbose -pkg $PKGROOT/SecUpd2007-006Univ.pkg -target /Volumes/$VOLNAME
fi


## Xcode Tools 2.4.0

sudo installer -verbose -pkg $PKGROOT/DeveloperTools.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/Java14Tools.pkg -target /Volumes/$VOLNAME

## Xcode SDKs

sudo installer -verbose -pkg $PKGROOT/DevSDK.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSX10.4.Universal.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSX10.4.Universal.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/BSDSDK.pkg -target /Volumes/$VOLNAME

sudo installer -verbose -pkg $PKGROOT/X11SDK.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSX10.3.9.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSX10.3.9.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSX10.4.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSX10.4.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/gcc3.3.pkg && \
sudo installer -verbose -pkg $PKGROOT/gcc3.3.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/gcc4.0.pkg && \
sudo installer -verbose -pkg $PKGROOT/gcc4.0.pkg -target /Volumes/$VOLNAME

## Xcode Legacy

test -r $PKGROOT/MacOSX10.1.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSX10.1.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/MacOSX10.2.8.pkg && \
sudo installer -verbose -pkg $PKGROOT/MacOSX10.2.8.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/gcc2.95.2.pkg && \
sudo installer -verbose -pkg $PKGROOT/gcc2.95.2.pkg -target /Volumes/$VOLNAME

test -r $PKGROOT/gcc3.1.pkg && \
sudo installer -verbose -pkg $PKGROOT/gcc3.1.pkg -target /Volumes/$VOLNAME


## MacPorts 1.4.0

sudo installer -verbose -pkg $PKGROOT/MacPorts-1.4.0.pkg -target /Volumes/$VOLNAME


## Installer leaves mounts open :-(

sudo umount /Volumes/$VOLNAME/dev
sudo umount /Volumes/$VOLNAME/dev

## Regular unmount normally fails :-(

hdiutil unmount /Volumes/$VOLNAME
hdiutil detach -Force /Volumes/$VOLNAME

exit 0
