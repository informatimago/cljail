PREFIX=/srv/ipl

all:
	@echo "$(MAKE) iplwcl-server"
	@echo "$(MAKE) install"

server:iplwcl-server
iplwcl-server: Makefile *.lisp
	ccl -norc < generate-server.lisp
install:iplwcl-server
	mkdir -p "$(PREFIX)/scripts" "$(PREFIX)/bin"
	cp -a siab run make-chroot     "$(PREFIX)/scripts"
	cp -a iplwcl-server show-users "$(PREFIX)/bin"
	sudo chown -R iplwcl:iplwcl "$(PREFIX)"

# TODO: scripts should be renamed libexec.
