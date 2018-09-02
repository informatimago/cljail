function darwin.makeChrootEnvironment(){
    local name="$1" ; shift
    local osx="/Volumes/OS X Install ESD/Packages"
    hdiutil create -fs HFS+ -volname "${name}" -verbose -size 4.5g "${name}.dmg"
    hdiutil mount "${name}.dmg"
    for pkg in BSD BaseSystemBinaries BaseSystemResources Essentials ; do
        installer \
            -pkg "${osx}/${pkg}.pkg" \
            -target "/Volumes/${name}"
    done
    umount "/Volumes/${name}/dev"
    umount "/Volumes/${name}/dev"
    hdiutil unmount       "/Volumes/${name}"
    hdiutil detach -Force "/Volumes/${name}"
}
